#' ##############################
#' Process raw FF relaunch data
#' 
#' ##############################


#load libraries
library(raster)
library(sf)
library(fasterize)
library(spex)
library(dplyr)
library(purrr)
library(furrr)
library(lwgeom)
library(stringr)

options(stringsAsFactors = FALSE)


source("./_pipeline_functions.R")

read_dir <-  "../../../data/relaunch_data/level_0/"
write_dir <-  "../../../data/relaunch_data/level_0/raster_tiles/"

#read data
sf_objects <- list.files(read_dir) %>%
  str_subset("utm_zone") %>%
  str_c(read_dir, .) %>%
  map(readRDS)

##
# function to make a consensus raster tile ####
# from one subject - an sf object
# make into a raster at the 10m scale by default 
# so that it doesn't kill spex
##

#subj <- unique(sf_objects[[1]]$subject_ids)
#one_subject <- sf_objects[[1]] %>% filter(subject_ids == subj[1])

rasterize_one_subject <- function(one_subject, res = 10, write_out_tile = TRUE){
  cat(str_c("Rasterizing ", one_subject$subject_ids[1], "...\n"))
  
  #make sure this isn't a blank tile
  one_subject_nonempty <- dplyr::filter(one_subject, !(sapply(one_subject$geometry, is_empty)))

  #if it's all empty, create an empty raster
  #using subject as the bbox
  if(nrow(one_subject_nonempty)==0){
    
    one_line <- one_subject[1,]
    

    rast <- raster(crs = st_crs(one_subject), 
           res = res, 
           xmn = as.numeric(one_line$`#tile_UL_x`),
           xmx = as.numeric(one_line$`#tile_LR_x`),
           ymn = as.numeric(one_line$`#tile_LR_y`),
           ymx = as.numeric(one_line$`#tile_UL_y`))
    
  
  }else{
    rast <- fasterize(one_subject %>% mutate(value = 1), 
              raster(one_subject, res = 10), 
              field = "value", fun = "sum")
  }
  
  #write out the tile in the meanwhile
  if(write_out_tile){
    #supressing warnings for blank rasters
    suppressWarnings(
      out <- writeRaster(rast, str_c(write_dir, one_subject$subject_ids[1], ".grd"), overwrite=TRUE)
    )
  }
  

    return(rast)
}


##
# Pipeline:
# for each data file
# and for each subject, 
# make a consensus raster tile
##

# testing code
# 
# set.seed(5000)
# levs <- unique(sf_objects[[1]]$subject_ids) %>% sample(10, replace=FALSE)
# 
# test <- sf_objects[[1]] %>%
#   filter(subject_ids %in% levs)
# 
# test_rast <- map(split(test, test$subject_ids), rasterize_one_subject)


rasters <- walk(sf_objects, ~
                  map(split(.x, .x$subject_ids), rasterize_one_subject)) 