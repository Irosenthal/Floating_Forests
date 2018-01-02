source("./FFmethods_fix.R")
library(R.matlab)
library(rgdal)
library(readr)
library(dplyr)
library(beepr)

#First, create a dataframe to nest spatial poly data frames in
classifications <- readRDS("../data/products/classifications_master")
beep(1)
subjects <- readRDS("../data/products/subjects_master") 
beep(1)
scene_meta <- read_csv("../data/products/scene_metadata.csv") 
beep(1)

saveRDS(subjects, "../data/products/subjects_master")


subjects <- subjects %>%
   dplyr:: select(zooniverse_id, UTM_ZONE)

#so it plays nice with some functions  FFmethods_fix, streamline this in the future
#remnant from old pipeline
scene_with_FF_UTM <- subjects


classifications <- left_join(classifications, subjects, by = c("subject_zooniverse_id" = "zooniverse_id"))

# for playing with 1 scene at a time
classifications <- filter(classifications, as.character(utm) == "LE70440351999204EDC01")
classifications <- filter(classifications, as.character(UTM_ZONE) == "10")



#remove any rows that have NAs
classifications_clean <- rm_na(classifications, "relPath")
classifications_clean <- rm_na(classifications_clean, "startingPoint_x")
classifications_clean <- rm_na(classifications_clean, "startingPoint_y")
classifications_clean <- mutate(classifications_clean, upper_right_x = tile_upper_right_x_UTM)
classifications_clean <- mutate(classifications_clean, upper_right_y = tile_upper_right_y_UTM)
classifications_clean <- mutate(classifications_clean, lower_left_x = tile_lower_left_x_UTM)
classifications_clean <- mutate(classifications_clean, lower_left_y = tile_lower_left_y_UTM)
classifications_clean <- rm_na(classifications_clean, "upper_right_x")
classifications_clean <- rm_na(classifications_clean, "upper_right_y")
classifications_clean <- rm_na(classifications_clean, "lower_left_x")
classifications_clean <- rm_na(classifications_clean, "lower_left_y")


#make a dataframe where each image has its classification data nested within it

nested_classifications <- split(classifications_clean , f = classifications_clean$subject_zooniverse_id)

#now lapply to convert to SPDFS
#This is where you need a subject table called scene_with_FF_UTM
sp_classifications_list <- lapply(nested_classifications, getSpatialPolysDataFrameForOneImage)
#saveRDS(sp_classifications_list, "../data/intermediates/nested_classifications_spatial")

sp_classifications_df <- data.frame(SPDF = matrix(ncol = 0, nrow = length(sp_classifications_list)))

sp_classifications_df$SPDF <- sp_classifications_list

saveRDS(sp_classifications_df, "../data/products/sp_classifications_df.rds")
classdf <- readRDS("../data/products/sp_classifications_df.rds")
