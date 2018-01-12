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

#####
# Load subject metadata
#####

subjects_metadata <- as.tibble(readRDS("../../data/output/raw_data_pipeline/subjects_master.rds"))

#prjs <- unique(sapply(kelp_spdf$SPDF, function(x) proj4string(x)))

###########
##### Functions
###########

make_rast <- function(a_spdf){
  combnR <- raster(crs=a_spdf@proj4string, ext=extent(a_spdf))
  rastLayerCombn <- rasterize(SpatialPolygons(a_spdf@polygons), combnR, 
                              fun="count")  
  
  rastLayerCombn
}

get_date <- function(a_spdf) a_spdf@data$DATE_ACQUIRED[1]

get_shared_areas <- function(rasts){
  merged <- do.call(merge, rasts)
}

get_spdf <- function(arast){
  thresholds <- na.exclude(unique(values(arast)))
  spdf_list <- lapply(thresholds, function(x){
    temp_rast <- arast
    values(temp_rast)[values(temp_rast)<x] <- NA
    values(temp_rast)[!is.na(values(temp_rast))] <- x
    #as(temp_rast, "SpatialPolygonsDataFrame")
    rasterToPolygons(temp_rast, dissolve = TRUE)
  })
  
  merged_spdf <- do.call(rbind, spdf_list)
  
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
  
  kelp_spdf <- as.tibble(readRDS(paste0(datadir, filename)))
  
  #for debug
  #kelp_spdf <- kelp_spdf[1:10,]
  
  print(paste0("rasterizing and unrasterizing", filename))
  
  #the long part - rasterizing and un-rasterizing
  spatial_df <- kelp_spdf %>% 
  group_by(1:n()) %>%
  mutate(dates = get_date(SPDF[[1]]))%>%
  nest() %>%
  mutate(rasts = purrr::map(data, ~make_rast(.$SPDF[[1]]))) %>%
  mutate(spdfs = purrr::map(rasts, ~get_spdf(.[[1]]))) 

  #saved spatial_df at this point - can load from .rds
  #saveRDS(spatial_df, file="../agu_data/spatial_df.rds")
  #spatial_df <- readRDS("../agu_data/spatial_df.rds")
  null_idx <- which(sapply(spatial_df$spdfs, is.null))
  spatial_df_clean <- spatial_df
  
  if(length(null_idx)>0)
    spatial_df_clean <- spatial_df[-which(sapply(spatial_df$spdfs, is.null)),]
  
  #assigning things - quicker processing part
  print(paste0("adding data back to spdf for", filename))
  
  spatial_df_clean <- spatial_df_clean %>%
    mutate(spdfs = purrr::map2(spdfs, data, ~add_data(.x, .y$SPDF[[1]], "subject_zooniverse_id"))) %>%
    unnest(data) %>%
    ungroup() 

  #Bunch of nulls? Check later

  #######
  #bring it all together into one SpatialPolygonsDataFrame
  #######
  print(paste0("making master spdf for", filename))
  
  all_spdfs_together <- do.call(rbind, spatial_df_clean$spdfs)
  #names(all_spdfs_together@data[1]) <- "threshold"

  all_spdfs_together@data <- all_spdfs_together@data %>%
    rename(threshold = layer,
         zooniverse_id = subject_zooniverse_id)

  ########
  #merge in zooniverse subject metadata
  #########
  print(paste0("merging subject metadata for", filename))
  
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
                                 return_spdf=FALSE){
  
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
  
  # if(coastcrop){
  #   ca_coastline <- readOGR("../../data/coastlines/California", layer="California")
  #  # all_spdfs_together_cropped <- crop(all_spdfs_together, ca_coastline, inverse=TRUE)
  #   #all_spdfs_together_cropped <- all_spdfs_together[-is.na(together[,1]),]
  #   clip <- gDifference(all_spdfs_together, ca_coastline, byid=TRUE)
  #   row.names(clip) <- gsub(" .*", "", row.names(clip))
  #   #clip <- spChFIDs(clip, row.names(clip))
  #   
  #   
  #   all_spdfs_together_cropped <- SpatialPolygonsDataFrame(clip,
  #                                                          data=all_spdfs_together@data)
  #   #all_spdfs_together_cropped <- erase(all_spdfs_together,ca_coastline)
  #   
  #   plot(all_spdfs_together_cropped %>% filter(threshold==1), border="red")
  #   plot(ca_coastline, add=TRUE)
  # }
  
  
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

make_consensus_files(filename = "sp_classifications_df_zone_10.rds")
make_consensus_files(filename = "sp_classifications_df_zone_11.rds")
make_consensus_files(filename = "sp_classifications_df_zone_55.rds")


