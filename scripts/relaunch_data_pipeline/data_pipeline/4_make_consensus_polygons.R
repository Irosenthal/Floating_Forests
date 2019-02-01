#' ##############################
#' Make sf polygons from consensus rasters
#' 
#' ##############################


#load libraries
library(sf)
library(fasterize)
library(spex)
library(dplyr)
library(purrr)
library(furrr)
library(stringr)
library(lwgeom)
library(tictoc)
options(stringsAsFactors = FALSE)
source("./_pipeline_functions.R")

#setup multicore environment to use all cores available ####
plan(multiprocess)
#plan(sequential) #for testing

read_dir <-  "../../../data/relaunch_data/level_0/"
write_dir <-  "../../../data/relaunch_data/level_0/"


#read data
ff_relaunch_subjects <- readRDS(str_c(read_dir, "ff_relaunch_subjects.rds"))

#get list of raster tiles
raster_tiles <- list.files(str_c(read_dir, "raster_tiles/")) %>%
  str_subset("grd") %>%
  str_c(read_dir, "raster_tiles/", .)



##
# Functions for polygonization ####
##

#get threshold rasters
make_threshold_raster <- function(a_rast, threshold){
  a_rast <-   a_rast >= threshold
  raster::values(a_rast)[!raster::values(a_rast)] <- NA
  # raster::values(a_rast)[raster::values(a_rast)] <- threshold
  a_rast
}

#spex::polygonize sometimes throws odd errors, so...
safe_polygonize <- function(a_rast){
  poly <- try(polygonize(a_rast), silent=TRUE)
  if(class(poly)[1]=="try-error"){
    poly <- st_as_sfc(raster::rasterToPolygons(a_rast))
  }
  
  return(poly)
}

make_threshold_polygons <- function(a_rast, max_threshold = 13){
  out_polys <- map(1:max_threshold, ~make_threshold_raster(a_rast, .x)) %>%
    map(safe_polygonize) 

  #unionize into sfg objects by threshold
  out_union <-  map(out_polys, st_union)

  #make into sf object
  op_mp <- out_union %>%
    do.call(c, .) %>%
    st_sf(threshold = 1:13, geometry = .) %>%
    st_make_valid() 
  
  #return the sf object
  return(op_mp)
}

make_threshold_polys_from_tilepath <- function(tile_path, max_threshold = 13, write_out = TRUE,
                                               write_dir = "", tile_out = "sf_tiles/"){
  a_rast <- raster(tile_path)
  
  subj_id <- str_extract(tile_path, "\\d{2,}")
  
  #debug
  tic()
  cat("Starting to polygonize ", subj_id, "...\n")
  
  thresh_sf <- make_threshold_polygons(a_rast, max_threshold = max_threshold) %>%
    mutate(subject_id = subj_id)
  
  #write out just in case
  if(write_out){saveRDS(thresh_sf, str_c(write_dir, tile_out, subj_id, ".Rds"))}
  
  #debug
  time_out <- toc(quiet = TRUE)
  
  cat(subj_id, " done after ", time_out$toc - time_out$tic, " seconds\n")
  
  return(thresh_sf)
  
}


#filter out any tiles already done
cat("Filtering done tiles...\n")
done_tiles <- list.files(str_c(write_dir, "sf_tiles/")) %>%
  str_replace("\\.Rds", "")

#if some are done...
if(length(done_tiles)>0){
  raster_tiles_filtered <- raster_tiles %>%
    discard(str_detect(., done_tiles))
}else{
  raster_tiles_filtered <- raster_tiles
}

#make the consensus polygon tiles
#install furrr from github for future_walk instead
if(length(raster_tiles_filtered)>0){
  cat("Making consensus polygons...\n")
  consensus_tiles <- future_walk(raster_tiles_filtered, make_threshold_polys_from_tilepath, 
                       write_dir = write_dir, .progress=TRUE)
}else{
  cat("No consensus rasters to parse\n")
}

# testing code below
#
# test <- keep(raster_tiles, str_detect(raster_tiles, "15123197"))
# make_threshold_polys_from_tilepath(test, write_dir = write_dir)

# tilename <- consensus_tiles[[2]]$subject_id[1]
# tile <- rasterizeFFImage(tilename)
# raster::plotRGB(tile)
# plot(consensus_tiles[[2]]["threshold"], add=TRUE)
