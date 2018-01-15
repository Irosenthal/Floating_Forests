###########
# Libraries
###########

library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(lubridate)
library(tidyverse)
library(spdplyr)
library(future)


#####
# Get ready for multicore
#####
plan(multiprocess)
#options(future.availableCores.system = availableCores()-1)
options(future.availableCores.system = 40)
#options(future.globals.maxSize= 1500*1024^2) #for the rasterize function

#####
# Load subject metadata
#####

#moved to inside of function where it's used as it hoses memory usage
#subjects_metadata <- as.tibble(readRDS("../../data/output/raw_data_pipeline/subjects_master.rds"))

#prjs <- unique(sapply(kelp_spdf$SPDF, function(x) proj4string(x)))

###########
##### Functions
###########

make_rast <- function(a_spdf){
  print(paste0("making raster for ", a_spdf@data$subject_zooniverse_id[1]))
  combnR <- raster(crs=a_spdf@proj4string, ext=extent(a_spdf))
  rastLayerCombn <- rasterize(SpatialPolygons(a_spdf@polygons), combnR, 
                              fun="count")  
  
  #fix the names for the coming rbind
  names(rastLayerCombn) <- a_spdf@data$subject_zooniverse_id[1]
  
  rastLayerCombn
}

get_date <- function(a_spdf) a_spdf@data$DATE_ACQUIRED[1]

get_shared_areas <- function(rasts){
  merged <- do.call(merge, rasts)
}

get_spdf <- function(arast){
  print(paste0("making spdf for ", names(arast)))
  
  thresholds <- na.exclude(unique(raster::values(arast)))
  spdf_list <- lapply(thresholds, function(x){
    temp_rast <- arast
    raster::values(temp_rast)[raster::values(temp_rast)<x] <- NA
    raster::values(temp_rast)[!is.na(raster::values(temp_rast))] <- x
    #as(temp_rast, "SpatialPolygonsDataFrame")
    rasterToPolygons(temp_rast, dissolve = TRUE)
  })
  
  merged_spdf <- do.call(rbind, spdf_list)
  
  names(merged_spdf@data)[1] <- "threshold"
  
  merged_spdf
  
}

add_data <- function(target_spdf, reference_spdf, colname){
  target_spdf@data[[colname]] <- reference_spdf@data[[colname]][1]
  
  target_spdf
}

#dates <- lapply(kelp_spdf$SPDF, get_date)
#rasts <- sapply(kelp_spdf$SPDF[1:10], make_rast )

###########
#### Create an aggregated data frame
#### with spdfs at different user thresholds
###########


#kelp_spdf <- as.tibble(readRDS("../../data/output/raw_data_pipeline/sp_classifications_df.rds"))

make_consensus_spdf <- function(filename,
                                datadir = "../../data/output/raw_data_pipeline/",
                                plotme=FALSE){
  
  kelp_spdf <- as.tibble(readRDS(paste0(datadir, filename)))[1:5,]
  
  #for debug
  #kelp_spdf <- kelp_spdf[1:10,]
  
  print(paste0("rasterizing and unrasterizing", filename))
  
  #the long part - rasterizing and un-rasterizing
  spatial_df <- kelp_spdf %>% 
    group_by(1:n()) %>%
    mutate(dates = get_date(SPDF[[1]])) %>%
    ungroup() %>%
    mutate(rasts = purrr::map(SPDF, ~future(make_rast(.x)))) %>%
    mutate(rasts = purrr::map(rasts, ~future::values(.x))) %>%
    mutate(spdfs = purrr::map(rasts, ~future(get_spdf(.x)))) %>%
    mutate(spdfs = purrr::map(spdfs, ~future::values(.x)))
  
  
  #saved spatial_df at this point - can load from .rds
  #saveRDS(spatial_df, file="../agu_data/spatial_df.rds")
  #spatial_df <- readRDS("../agu_data/spatial_df.rds")
  null_idx <- which(sapply(spatial_df$spdfs, is.null))
  spatial_df_clean <- spatial_df
  
  if(length(null_idx)>0)
    spatial_df_clean <- spatial_df[-which(sapply(spatial_df$spdfs, is.null)),]
  
  #assigning things - quicker processing part
  print(paste0("adding data back to spdf for ", filename))
  
  spatial_df_clean <- spatial_df_clean %>%
    mutate(spdfs = purrr::map2(spdfs, SPDF, ~add_data(.x, .y, "subject_zooniverse_id")))

  
  tmp <- tmp %>%
    mutate(spdfs = purrr::map(spdfs, ~names(.x@data)[1] = "threshold"))
  
  #Bunch of nulls? Check later

  #######
  #bring it all together into one SpatialPolygonsDataFrame
  #######
  print(paste0("making master spdf for ", filename))
  
  all_spdfs_together <- do.call(rbind, spatial_df_clean$spdfs)
  #names(all_spdfs_together@data[1]) <- "threshold"

  all_spdfs_together@data <- all_spdfs_together@data %>%
    rename(zooniverse_id = subject_zooniverse_id)

  ########
  #merge in zooniverse subject metadata
  #########
  print(paste0("merging subject metadata for", filename))

  #putting this here to minimize memory use
  subjects_metadata <- as.tibble(readRDS("../../data/output/raw_data_pipeline/subjects_master.rds"))
  
  all_spdfs_together@data <- left_join(all_spdfs_together@data, 
          subjects_metadata %>% select(zooniverse_id, scene, classification_count,
                                       location.standard, 
                                       UTM_ZONE:tile_lower_right_y_LL,
                                       metadata.timestamp,
                                       activated_at, state) %>%
            rename(image_url = location.standard,
                   scene_timestamp = metadata.timestamp))


  #######
  # test with plots
  # note use of spdplyr
  #######

  if(plotme){
    plot(all_spdfs_together %>% filter(threshold==1), border="red")
    plot(all_spdfs_together %>% filter(threshold==4), border = "blue", add=TRUE)
    plot(all_spdfs_together %>% filter(threshold==6), border = "brown", add=TRUE)
  }
  
  all_spdfs_together
}

#######
# write out a shapefile function
#######
make_consensus_files <- function(filename,
                                 datadir = "../../data/output/raw_data_pipeline/",
                                 plotme = FALSE,
                                 reproject = TRUE,
                                 outdir =  "../../data/output/consensus_shapefiles/",
                                 rds = TRUE,
                                 sqlite = TRUE,
                                 return_spdf=FALSE,
                                 coastcrop=NULL){
  
  print(paste0("Starting ", filename))
  
  #make the consensus spdf
  all_spdfs_together <- make_consensus_spdf(filename, datadir, plotme)
  
  #rowname fix
  rownames(all_spdfs_together@data) <- row.names(all_spdfs_together)
  
  print(paste0("Made consensus spdf for ", filename))
  

  if(reproject){
    print(paste0("reprojecting spdf for", filename))
    
    all_spdfs_together <- spTransform(all_spdfs_together, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    all_spdfs_together <- all_spdfs_together %>%
      select(-c(UTM_ZONE:tile_upper_right_y_UTM))
  }else{    
    all_spdfs_together <- all_spdfs_together %>%
      select(-c(tile_upper_left_x_LL:tile_lower_right_y_LL))
  }
  
  if(!is.null(coastcrop)){
    print(paste0("cropping out coast from", filename))
    
    all_spdfs_together <- erase(all_spdfs_together, coastcrop)

   # plot(all_spdfs_together %>% filter(threshold==1), border="red")
    #plot(ca_coastline, add=TRUE)
  }
  
  
  outfilename <- gsub("sp_classifications_df", "ff_consensus_polys", filename)
  outfilename <- gsub("\\.rds", "", outfilename)

  
  if(sqllite) {
    print(paste0("writing sqlite spdf for", filename))
    writeOGR(all_spdfs_together, paste0(outdir,outfilename,".sqlite"), "ff_consensus", driver="SQLite", overwrite_layer=TRUE)
  }
  
  if(rds){
    print(paste0("writing rds for", filename))
    saveRDS(all_spdfs_together, file=paste0(outdir,outfilename,".rds"))
  }  
  
  if(return_spdf) return(all_spdfs_together)
  
  print(paste0("Done with", filename))
  
  return(TRUE)
}

#######
# make it so
#######
ca_coastline <- readOGR("../../data/coastlines/California", layer="California")
tazzie_coastline <- readOGR("../../data/coastlines/tasmania", layer="Tasmania")

make_consensus_files(filename = "sp_classifications_df_zone_10.rds",
                     coastcrop=ca_coastline)
make_consensus_files(filename = "sp_classifications_df_zone_11.rds",
                     coastcrop=ca_coastline)
make_consensus_files(filename = "sp_classifications_df_zone_55.rds",
                     coastcrop=tazzie_coastline)


