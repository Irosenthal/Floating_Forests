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
library(tidyv)



#read data
classifications <- readRDS("../../data/output/raw_data_pipeline/level_0/zone_55_classifications.rds")
classifications$user_name <- as.factor(classifications$user_name) 
#set write out directory for rasters
write_dir <- "../../data/output/raw_data_pipeline/level_0/raster_tiles/"

##################################
#subsets for testing
#don't need to make these for actual pipeline 
#I wanted to keep them around for troubleshooting

#this is a known problematic subject
bad_sub <- classifications %>%
  filter(subject_zooniverse_id == "AKP0005ix0")
st_geometry_type(bad_sub)
#smaller subset that contains several complete subjects
classifications2 <- classifications[1:50,]
#filter out everything that isn't a multipolygon (this is aggressive but overall drops few subjects)
classifications_filtered <- classifications %>%
  filter(st_geometry_type(classifications$geometry) == "MULTIPOLYGON")

###################
# function to make a consensus raster tile ####
# from one subject - an sf object
# make into a raster at the 10m scale 

rasterize_one_subject <- function(one_subject, res = 10, write_out_tile = TRUE){
  print(one_subject$subject_zooniverse_id[1])
  #all non-multipolygons removed, but some multipolygons are just redudant sets of vertices.
  one_subject_coords <- st_coordinates(one_subject)[,1:2]
  
  #If all coordinates are identical the classification is invalid
  #only rasterize valid polys, make blank rasters for the rest
  if(!length(unique(one_subject_coords)) == 2)
    #rasterize
    rast <- fasterize(one_subject,
                      raster(one_subject, nrows = 400, ncols = 364),
                      field = "user_name",
                      fun = "count") %>%
    writeRaster(str_c(write_dir, one_subject$subject_zooniverse_id[1], ".grd"), overwrite=TRUE)
  
  else
    #make an empty raster for bad subs
    rast <- raster(nrows = 400, ncols = 364) %>%
    #write out with flag for bad sub
    writeRaster( str_c(write_dir,"bad_", one_subject$subject_zooniverse_id[1], ".grd"), overwrite=TRUE)
  
  
  return(rast)
}

#####################

#run it
test_rast <- map(split(classifications, classifications$subject_zooniverse_id), rasterize_one_subject)

#check a few
raster_check <- raster("../../data/output/raw_data_pipeline/level_0/raster_tiles/AKP0000bp3.gri")
plot(raster_check)

