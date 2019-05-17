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

options(stringsAsFactors = FALSE)

#setup multicore environment to use all cores available ####
plan(multiprocess)
plan(sequential) #for testing

source("./_pipeline_functions.R")

write_dir <- "../../data/output/raw_data_pipeline/level_0/raster_tiles/"

#read data
classifications <- readRDS("../../data/output/raw_data_pipeline/level_0/zone_11_classifications.rds")
classifications$user_name <- as.factor(classifications$user_name) 


##
# function to make a consensus raster tile ####
# from one subject - an sf object
# make into a raster at the 10m scale by default 
# so that it doesn't kill spex
##

#subj <- unique(sf_objects[[1]]$subject_ids)
one_subject <- classifications %>% 
  filter(subject_zooniverse_id == subject_zooniverse_id[1])
#
st_get_npts <- function(x){
  sapply(x, function(x) nrow(x[[1]]))
}

st_get_npts_sum <- function(x){
  pts <- st_get_npts(x)
  sum(pts)
}

st_get_npoly <- function(x){
  length(x)
}

#some multipolygons have places where a user
#just clicked on a point or a line
st_remove_points <- function(x){
  pts <- which(st_get_npts(x)>3)
  if(length(pts)==length(x)) return(x) #safeguard
  
  # map(pts, ~pluck(x, .)) %>%
  #   st_multipolygon
  valid_geom <- st_make_valid(x) %>%
    st_cast(to = "GEOMETRYCOLLECTION") 
  
  geom_pieces <- map_lgl(valid_geom, ~  1  <= sum(str_detect(class(.x), "POLYGON")))

  #in case there was nothing
  if(sum(geom_pieces) == 0) return(st_multipolygon())
  
  #in case it's 1 polygon
  valid_geom <- valid_geom[geom_pieces][[1]]
  if(!("MULTIPOLYGON" %in% class(valid_geom)))
    valid_geom <- st_cast(valid_geom, "MULTIPOLYGON")
  
  #return!   
  return(valid_geom) 
}

rasterize_one_subject <- function(one_subject, res = 10, write_out_tile = TRUE){
  #cat(str_c("Rasterizing ", classifications$subject_zooniverse_id[[1]], "...\n"))
  print(one_subject$subject_zooniverse_id[1])
  #make sure this isn't a blank tile
  #one_subject_nonempty <- one_subject %>%
   # dplyr::filter(!(sapply(one_subject$geometry, is_empty))) %>%
    #mutate(geometry = map(geometry, st_remove_points),
 #           empt = map_lgl(geometry, st_is_empty)) %>%
#    dplyr::filter(!empt) %>%
 #   dplyr::select(-empt)
    
#rasterize based on these nonempty SFS: get it working with all, then add ifelse to have it kick out a blank raster when empty
  
  
  rast <- fasterize(one_subject, #give it an SF
                 raster(one_subject, nrows = 400, ncols = 364), #and a template for the raster
                 field = "user_name", #must be a factor (bug?)
                 fun = "count")
  

 writeRaster(rast, str_c(write_dir, one_subject$subject_zooniverse_id[1], ".grd"), overwrite=TRUE)
    
  }
  

    return(rast)
}
#####################
#testing 

bad_sub <- classifications %>%
  filter(subject_zooniverse_id == "AKP000059a")


classifications2 <- classifications[1:50,]
test_split <-split(classifications2, classifications2$subject_zooniverse_id)
test_rast <- map(test_split, rasterize_one_subject)


#ok, it's breaking when they are polygons instead of multipolygons.
#try dropping all polys

classifications_filtered <- classifications %>%
  filter(st_geometry_type(classifications$geometry) == "MULTIPOLYGON")



######


#run it
test_rast <- map(split(classifications_filtered, classifications_filtered$subject_zooniverse_id), rasterize_one_subject)

#check a few
raster_check <- raster("../../data/output/raw_data_pipeline/level_0/raster_tiles/AKP000050o.grd")
plot(raster_check)

#filter out any tiles already done
done_tiles <- list.files(write_dir) %>%
  str_replace("\\.gr[d,i]", "") %>%
  unique()

sf_objects_todo <- map(sf_objects, ~filter(.x, !(subject_ids %in% done_tiles)))

rasters <- walk(classifications, ~
                  map(split(.x, .x$subject_zooniverse_id), rasterize_one_subject)) 

