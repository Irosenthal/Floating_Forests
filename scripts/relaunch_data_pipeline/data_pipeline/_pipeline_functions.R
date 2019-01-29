#' ##############################
#' Helper and debugging functions
#' for FF data processing pipeline
#' ##############################


##
#Functions for dealing with metadata ####
##

get_subj_metadata <- function(one_row, workflow_id = 2150){
  subject <- fromJSON(one_row$subject_data)[[1]]
  
  ff_relaunch_subjects %>% 
    filter(subject_id == one_row$subject_ids) %>%
    filter(workflow_id == !!workflow_id)
}

get_utm_zone <- function(one_row){
  subj_metadata <- get_subj_metadata(one_row)
  
  fromJSON(subj_metadata$metadata)$`#utm_zone`
}

get_subj_metadata_from_JSON <- function(one_row, workflow_id = 2150){
  subj_metadata <- get_subj_metadata(one_row)
  
  fromJSON(subj_metadata$metadata)
  
}
#pmap_chr(ff_test, get_utm_zone) %>%  unique() %>% as.numeric()

####
## Functions for getting images from online
####
#gets an image from a row of FF data
rasterizeFFImage_from_row <- function(one_row){
  rasterizeFFImage(one_row$subject_ids)
}

rasterizeFFImage <- function(subject){
  subj_metadata <- ff_relaunch_subjects %>% filter(subject_id == subject)
  subj_url <- subj_metadata$locations

  crs <- str_c("+proj=utm +zone=",  subj_metadata$`#utm_zone`, 
               " +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  
  download.file(subj_url, basename(subj_url), mode = 'wb')
  
  img <- raster::brick(basename(subj_url),              
                       crs=crs)
  
  img@extent = raster::extent(as.numeric(subj_metadata$`#tile_UL_x`), as.numeric(subj_metadata$`#tile_LR_x`),
                              as.numeric(subj_metadata$`#tile_LR_y`), as.numeric(subj_metadata$`#tile_UL_y`))
  
  return(img)
  
}

##
## Turning single rows into sf objects
##

#a function to turn one subject data into a sf object
make_sfc_from_zoo <- function(one_classification, zone, 
                             ulx, uly, lrx, lry,
                             xsize, ysize){
  
  classification_list <- fromJSON(one_classification)
  scaling_x <- abs((ulx - lrx)/xsize)
  scaling_y <- abs((uly - lry)/ysize)
  
  #get the points
  polys <- as_tibble(classification_list$value[[1]])$points %>%
    #scale to image size in coordinates and add the corner coords
      map(~ .x %>% mutate(x = x * scaling_x + ulx,
                          y = abs(ysize - y) * scaling_y + lry)) %>%

    #turn into matrices
    map(as.matrix) %>%
    
    #turn into sf::polygons
    map(~st_polygon(list(.))) %>%
    
    #turn into one big sf::multipolygon
    st_multipolygon()
  
  
  #return and add CRS
  
  polys %>% st_sfc()#crs = str_c("+proj=utm +zone=", zone, " +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 
  
  
}


parse_row <- function(one_row, tile = NULL){
  subj_metadata <- get_subj_metadata(one_row)
  subj_spatial <- get_subj_metadata_from_JSON(one_row)
  subj_url <- fromJSON(subj_metadata$locations)[[1]]
  
  if(is.null(tile)) tile <- rasterizeFFImage_from_row(one_row)

  #debug
  if(length(subj_metadata)==0) stop("No metadata for subject")
  
  row_sfc <- make_sfc_from_zoo(one_row$annotations, 
                                 zone = subj_metadata$`#utm_zone`,
                             ulx = as.numeric(subj_spatial$`#tile_UL_x`),
                             uly = as.numeric(subj_spatial$`#tile_UL_y`),
                             lrx = as.numeric(subj_spatial$`#tile_LR_x`),
                             lry = as.numeric(subj_spatial$`#tile_LR_y`),
                             ysize = dim(tile)[1],
                             xsize = dim(tile)[2]
                             
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



make_subject_sf <- function(subject){
  #subset down to the data we want
  subject_subset <- ff_relaunch_classifications %>% filter(subject_ids==subject)
  tile <- rasterizeFFImage(subject_subset$subject_ids[1])
  
  #create sfc objects
  subject_sfc <- map(1:nrow(subject_subset), ~parse_row(subject_subset[.x,], tile = tile))
  
  #make into an sf object with the rows from the original data
  subject_sf <- do.call(c, subject_sfc) %>% 
    st_sf(classification_id = subject_subset$classification_id, geometry = .) %>% 
    left_join(subject_subset)
  
  #testing
  plotRGB(tile)
  plot(subject_sf$geometry, add=TRUE, col = "white")
  
  #return the new sf object
  subject_sf
}

####### testing
#all <- filter(ff_test, subject_ids==15113971)
# 
# b <- make_subject_sf(15113971)
# plotRGB(tile)
# plot(b$geometry, add=TRUE, col = "white")

# one_row <- all[9,]
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

