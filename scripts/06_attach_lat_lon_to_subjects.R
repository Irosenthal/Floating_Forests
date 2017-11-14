library(dplyr)
library(readr)
library(tidyr)
library(sp)


#load subjects
subjects <- read_csv("../data/products/subjects_master.csv")


#code to pull out all scenes with NA zones
zone_NA <- subjects %>%
  filter(is.na(UTM_ZONE))

#remove all subjects with NA coordinates. need to look into this later
subjects <- subjects %>%
  filter(!is.na(tile_upper_left_x_UTM))
subjects <- subjects %>%
  filter(!is.na(tile_upper_left_y_UTM))
subjects <- subjects %>%
  filter(!is.na(tile_lower_right_x_UTM))
subjects <- subjects %>%
  filter(!is.na(tile_lower_right_y_UTM))

#separate by UTM zones for conversion to long/lat
#do each zone separately and then aggregate them
zone_10 <- subjects %>%
  filter(UTM_ZONE == 10)

zone_11 <- subjects %>%
  filter(UTM_ZONE == 11)

zone_55 <- subjects %>%
  filter(UTM_ZONE == 55)


#select just the corners - make sure to note the utm zone for conversion to LL

#zone 10
upper_left_corners_zone_10 <- zone_10 %>%
  dplyr::select(c(tile_upper_left_x_UTM, tile_upper_left_y_UTM))
names(upper_left_corners_zone_10) <- c("tile_upper_left_x_LL", "tile_upper_left_y_LL")

lower_left_corners_zone_10 <- zone_10 %>%
  dplyr::select(c(tile_lower_left_x_UTM, tile_lower_left_y_UTM))
names(lower_left_corners_zone_10) <- c("tile_lower_left_x_LL", "tile_lower_left_y_LL")

upper_right_corners_zone_10 <- zone_10 %>%
  dplyr::select(c(tile_upper_right_x_UTM, tile_upper_right_y_UTM))
names(upper_right_corners_zone_10) <- c("tile_upper_right_x_LL", "tile_upper_right_y_LL")

lower_right_corners_zone_10 <- zone_10 %>%
  dplyr::select(c(tile_lower_right_x_UTM, tile_lower_right_y_UTM))
names(lower_right_corners_zone_10) <- c("tile_lower_right_x_LL", "tile_lower_right_y_LL")



#zone 11
upper_left_corners_zone_11 <- zone_11 %>%
  dplyr::select(c(tile_upper_left_x_UTM, tile_upper_left_y_UTM))
names(upper_left_corners_zone_11) <- c("tile_upper_left_x_LL", "tile_upper_left_y_LL")

lower_left_corners_zone_11 <- zone_11 %>%
  dplyr::select(c(tile_lower_left_x_UTM, tile_lower_left_y_UTM))
names(lower_left_corners_zone_11) <- c("tile_lower_left_x_LL", "tile_lower_left_y_LL")

upper_right_corners_zone_11 <- zone_11 %>%
  dplyr::select(c(tile_upper_right_x_UTM, tile_upper_right_y_UTM))
names(upper_right_corners_zone_11) <- c("tile_upper_right_x_LL", "tile_upper_right_y_LL")

lower_right_corners_zone_11 <- zone_11 %>%
  dplyr::select(c(tile_lower_right_x_UTM, tile_lower_right_y_UTM))
names(lower_right_corners_zone_11) <- c("tile_lower_right_x_LL", "tile_lower_right_y_LL")


#zone 55
upper_left_corners_zone_55 <- zone_55 %>%
  dplyr::select(c(tile_upper_left_x_UTM, tile_upper_left_y_UTM))
names(upper_left_corners_zone_55) <- c("tile_upper_left_x_LL", "tile_upper_left_y_LL")

lower_left_corners_zone_55 <- zone_55 %>%
  dplyr::select(c(tile_lower_left_x_UTM, tile_lower_left_y_UTM))
names(lower_left_corners_zone_55) <- c("tile_lower_left_x_LL", "tile_lower_left_y_LL")

upper_right_corners_zone_55 <- zone_55 %>%
  dplyr::select(c(tile_upper_right_x_UTM, tile_upper_right_y_UTM))
names(upper_right_corners_zone_55) <- c("tile_upper_right_x_LL", "tile_upper_right_y_LL")

lower_right_corners_zone_55 <- zone_55 %>%
  dplyr::select(c(tile_lower_right_x_UTM, tile_lower_right_y_UTM))
names(lower_right_corners_zone_55) <- c("tile_lower_right_x_LL", "tile_lower_right_y_LL")




#convert to spdfs
#zone 10
upper_left_corners_zone_10 <- SpatialPoints(upper_left_corners_zone_10,
                                            proj4string=CRS("+proj=utm +zone=10 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
lower_left_corners_zone_10 <- SpatialPoints(lower_left_corners_zone_10,
                                            proj4string=CRS("+proj=utm +zone=10 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 
upper_right_corners_zone_10 <- SpatialPoints(upper_right_corners_zone_10,
                                             proj4string=CRS("+proj=utm +zone=10 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 
lower_right_corners_zone_10 <- SpatialPoints(lower_right_corners_zone_10,
                                             proj4string=CRS("+proj=utm +zone=10 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 

#zone 11
upper_left_corners_zone_11 <- SpatialPoints(upper_left_corners_zone_11,
                                      proj4string=CRS("+proj=utm +zone=11 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 
lower_left_corners_zone_11 <- SpatialPoints(lower_left_corners_zone_11,
                                      proj4string=CRS("+proj=utm +zone=11 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 
upper_right_corners_zone_11 <- SpatialPoints(upper_right_corners_zone_11,
                                       proj4string=CRS("+proj=utm +zone=11 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
lower_right_corners_zone_11 <- SpatialPoints(lower_right_corners_zone_11,
                                       proj4string=CRS("+proj=utm +zone=11 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

#zone 55
upper_left_corners_zone_55 <- SpatialPoints(upper_left_corners_zone_55,
                                      proj4string=CRS("+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 
lower_left_corners_zone_55 <- SpatialPoints(lower_left_corners_zone_55,
                                      proj4string=CRS("+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 
upper_right_corners_zone_55 <- SpatialPoints(upper_right_corners_zone_55,
                                       proj4string=CRS("+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
lower_right_corners_zone_55 <- SpatialPoints(lower_right_corners_zone_55,
                                       proj4string=CRS("+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 
#reproject to LL
#zone 10
upper_left_corners_zone_10 <- spTransform(upper_left_corners_zone_10,"+proj=longlat +datum=WGS84")
lower_left_corners_zone_10 <- spTransform(lower_left_corners_zone_10,"+proj=longlat +datum=WGS84")
upper_right_corners_zone_10 <- spTransform(upper_right_corners_zone_10,"+proj=longlat +datum=WGS84")
lower_right_corners_zone_10 <- spTransform(lower_right_corners_zone_10,"+proj=longlat +datum=WGS84")

#zone 11
upper_left_corners_zone_11 <- spTransform(upper_left_corners_zone_11,"+proj=longlat +datum=WGS84")
lower_left_corners_zone_11 <- spTransform(lower_left_corners_zone_11,"+proj=longlat +datum=WGS84")
upper_right_corners_zone_11 <- spTransform(upper_right_corners_zone_11,"+proj=longlat +datum=WGS84")
lower_right_corners_zone_11 <- spTransform(lower_right_corners_zone_11,"+proj=longlat +datum=WGS84")

#zone 55
upper_left_corners_zone_55 <- spTransform(upper_left_corners_zone_55,"+proj=longlat +datum=WGS84")
lower_left_corners_zone_55 <- spTransform(lower_left_corners_zone_55,"+proj=longlat +datum=WGS84")
upper_right_corners_zone_55 <- spTransform(upper_right_corners_zone_55,"+proj=longlat +datum=WGS84")
lower_right_corners_zone_55 <- spTransform(lower_right_corners_zone_55,"+proj=longlat +datum=WGS84")



#return to dataframes
#zone 10
upper_left_corners_zone_10 <- as.data.frame(upper_left_corners_zone_10)
lower_left_corners_zone_10 <- as.data.frame(lower_left_corners_zone_10)
upper_right_corners_zone_10 <- as.data.frame(upper_right_corners_zone_10)
lower_right_corners_zone_10 <- as.data.frame(lower_right_corners_zone_10)

#zone 11
upper_left_corners_zone_11 <- as.data.frame(upper_left_corners_zone_11)
lower_left_corners_zone_11 <- as.data.frame(lower_left_corners_zone_11)
upper_right_corners_zone_11 <- as.data.frame(upper_right_corners_zone_11)
lower_right_corners_zone_11 <- as.data.frame(lower_right_corners_zone_11)


#zone 55
upper_left_corners_zone_55 <- as.data.frame(upper_left_corners_zone_55)
lower_left_corners_zone_55 <- as.data.frame(lower_left_corners_zone_55)
upper_right_corners_zone_55 <- as.data.frame(upper_right_corners_zone_55)
lower_right_corners_zone_55 <- as.data.frame(lower_right_corners_zone_55)


#cbind these back to the zone dfs

zone_10_complete <- cbind(zone_10, 
                          upper_left_corners_zone_10,
                          lower_left_corners_zone_10,
                          upper_right_corners_zone_10,
                          lower_right_corners_zone_10)

zone_11_complete <- cbind(zone_11, 
                          upper_left_corners_zone_11, 
                          lower_left_corners_zone_11,
                          upper_right_corners_zone_11,
                          lower_right_corners_zone_11)

zone_55_complete <- cbind(zone_55,
                          upper_left_corners_zone_55,
                          lower_left_corners_zone_55,
                          upper_right_corners_zone_55,
                          lower_right_corners_zone_55)

all_zones<- rbind(zone_10_complete, zone_11_complete, zone_55_complete)
all_zones_head <- head(all_zones)

#just spot check one to make sure this didn't create gibberish
image_check <- all_zones %>%
  dplyr::filter(zooniverse_id == "AKP00016e6")



#select zooniverse id for joining and the new ll columns
all_zones <- all_zones %>%
  dplyr::select(c(zooniverse_id, 
                  tile_upper_left_x_LL,
                  tile_upper_left_y_LL,
                  tile_lower_left_x_LL,
                  tile_lower_left_y_LL,
                  tile_upper_right_x_LL,
                  tile_upper_right_y_LL,
                  tile_lower_right_x_LL,
                  tile_lower_right_y_LL))
                  
#save this intermediate                 
write_csv(all_zones, "../data/intermediates/all_zones.csv")

#join new lon/lat corners on to master subjects table
subjects_full <- left_join(subjects, all_zones, by = "zooniverse_id")

#save into products folder
write_csv(subjects_full, "../data/products/subjects_master.csv")


