#' ##############################
#' Turn classification data into sf objects
#' 
#' ##############################


#load libraries
library(dplyr)
library(purrr)
library(furrr)
library(stringr)
library(sf)
library(rgdal)
library(raster)
options(stringsAsFactors = FALSE)

source("./_pipeline_functions.R")

read_dir <-  "../../../data/relaunch_data/level_0/"
write_dir <-  "../../../data/relaunch_data/level_0/"


#setup multicore environment to use all cores available ####
#plan(multiprocess)
plan(sequential) #for testing


#read data ####
ff_relaunch_classifications <- readRDS(str_c(read_dir, "floating_forests_classifications.rds"))

ff_relaunch_subjects <- readRDS(str_c(read_dir, "ff_relaunch_subjects.rds"))

#make a test data set
set.seed(5000)
levs <- unique(ff_relaunch_classifications$subject_ids) %>% sample(10, replace=FALSE)
ff_test <- ff_relaunch_classifications %>%
  filter(subject_ids %in% levs)


#a function to make sure the polygons are closed
close_poly <- function(a_poly){
  if(sum(a_poly[1,] == a_poly[nrow(a_poly),]) < 2){
    a_poly <- rbind(a_poly, a_poly[1,])
  }
  
  a_poly
}

#a function to turn one subject data into a sf object
make_sfc_from_zoo <- function(one_classification, zone, 
                             ulx, uly, lrx, lry,
                             xsize, ysize){
  
  classification_list <- jsonlite::fromJSON(one_classification)
  
  scaling_x <- abs((ulx - lrx)/xsize)
  scaling_y <- abs((uly - lry)/ysize)
  
  #If the user marked nothing...return an empty multipolygon
  if(length(classification_list$value[[1]])==0){
    #make an empty polygon
    return(st_multipolygon() %>% 
             st_sfc(crs = str_c("+proj=utm +zone=", zone, " +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  }
  
  #get the points
  polys <- as_tibble(classification_list$value[[1]])$points %>%
    #scale to image size in coordinates and add the corner coords
      map(~ .x %>% mutate(x = x * scaling_x + ulx,
                          y = abs(ysize - y) * scaling_y + lry)) %>%

    #turn into matrices
    map(as.matrix) %>%
    
    #turn into sf::polygons
    map(~ .x %>% close_poly() %>% list() %>% st_polygon()) %>%
    
    #turn into one big sf::multipolygon
    st_multipolygon()
  
  
  #return with crs
  
  polys %>% 
    st_sfc(crs = str_c("+proj=utm +zone=", zone, " +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 

}



#all <- filter(ff_test, subject_ids==15116735)
#one_row <- all[1,]
#one_classification <- one_row$annotations
#classification_list <- fromJSON(one_classification)


parse_row <- function(one_row){
  #debug output
  cat(str_c("     classification_id ", one_row$classification_id[1], "\n"))
  
  #get the sfc multipolygon  based on prroperties of the subject
  row_sfc <- make_sfc_from_zoo(one_row$annotations, 
                                 zone = one_row$`#utm_zone`,
                             ulx = as.numeric(one_row$`#tile_UL_x`),
                             uly = as.numeric(one_row$`#tile_UL_y`),
                             lrx = as.numeric(one_row$`#tile_LR_x`),
                             lry = as.numeric(one_row$`#tile_LR_y`),
                             ysize = as.numeric(one_row$naturalHeight),
                             xsize = as.numeric(one_row$naturalWidth)
                             
  )
  
  row_sfc
  
  #for testing
  #tile <- rasterizeFFImage_from_row(one_row)
  
  # test
  # plotRGB(tile)
  # plot(row_sf, add=TRUE, col = "white")
  
  #test with ggplot
  # a <- ggplot(row_sf) + geom_sf() + coord_sf(datum = st_crs(row_sf)) + xlim(c(308694.8, 319193.5)) + ylim(-5765380, -5754881)
  #  b <- RStoolbox::ggRGB(tile)
  #  a + b
  
}



make_subject_sf <- function(subject_subset){
  #debug output
  cat(str_c("Making ", subject_subset$subject_ids[1], " into sf\n"))
  
  #some metadata is missing from some classifications - fix this
  subject_subset <- subject_subset %>%
    mutate(naturalHeight = ifelse(is.na(naturalHeight), max(naturalHeight, na.rm=T), naturalHeight),
           naturalWidth = ifelse(is.na(naturalWidth), max(naturalWidth, na.rm=T), naturalWidth))
  
  #create sfc objects
  subject_sfc <- map(1:nrow(subject_subset), ~parse_row(subject_subset[.x,]))
  
  #make into an sf object with the rows from the original data
  suppressMessages(
   subject_sf <- do.call(c, subject_sfc) %>% 
    st_sf(classification_id = subject_subset$classification_id, geometry = .) %>% 
    left_join(subject_subset)
  )
  #testing
  # tile <- rasterizeFFImage(subject_subset$subject_ids[1])
  # plotRGB(tile)
  # plot(subject_sf$geometry, add=TRUE, col = "white")
  
  #return the new sf object
  subject_sf
}

####### testing
# subject_subset <- filter(ff_test, subject_ids==15116840)
# 
# b <- make_subject_sf(subject_subset)
# tile <- rasterizeFFImage(15113971)
# plotRGB(tile)
# plot(b$geometry, add=TRUE, col = "white")
# 
# one_row <- subject_subset[9,]
# one_classification <- one_row$annotations
# classification_list <- fromJSON(one_classification)
# 
# 
# polys <- as_tibble(classification_list$value[[1]])$points %>%
#   #scale to image size in coordinates
#  # map(~ .x %>% mutate(x = x* scaling_x, y = y*scaling_y)) %>%
#   
#   #add the corner coords
#   map(~ .x %>% mutate(x = abs(0-x) , y = abs(500-y) )) %>%
#   
#   
#   #turn into matrices
#   map(as.matrix) %>%
#   
#   #turn into sf::polygons
#   map(~st_polygon(list(.))) %>%
#   
#   #turn into one big sf::multipolygon
#   st_multipolygon()
# 
# plotRGB(tile)
# plot(polys, add=TRUE, col = "white")

#~~~~~~~~~~~~~~~~~~~~~~~~~
# The Pipeline ####
# for going from tibbles
# to lists of sf objects
# separated by utm zone
#~~~~~~~~~~~~~~~~~~~~~~~~~
# 

#TODO - insert code to check if a subject has already been reprocessed
#in which case, filter it out
#test code
# ff_20 <- filter(ff_test, `#utm_zone` == 20)
# a <- map(split(ff_20, ff_20$subject_ids), make_subject_sf) %>%
#   reduce(rbind)

#future at the level of subject ids
set.seed(5000)
levs <- unique(ff_relaunch_classifications$subject_ids) %>% sample(100, replace=FALSE)
ff_test <- ff_relaunch_classifications %>%
  filter(subject_ids %in% levs)

sf_list <- map(split(ff_test, ff_test$`#utm_zone`),
                ~future_map(split(.x, .x$subject_ids), make_subject_sf, .progress=TRUE) %>%
                 reduce(rbind))


#~~~~~~~~~~~~~~~~~~~~~~~~~
# Write out each zone ####
# as a Level 0 product
#~~~~~~~~~~~~~~~~~~~~~~~~~

#TODO - something to check if zone exists and merge in new subjects
save_utm_zone_rds <- function(sf_obj){
  zone <- sf_obj$`#utm_zone`[1]
  saveRDS(sf_obj, str_c(write_dir, "utm_zone_", zone, "_sf.Rds"))
}

walk(sf_list, save_utm_zone_rds)

