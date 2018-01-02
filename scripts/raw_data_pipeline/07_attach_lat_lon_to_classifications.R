########################################
####code to take raw classifications, trim off unnecessary data, and join on correct corners)
########################################

#load raw classifications
classifications <- read_csv("../data/2016-05-29_kelp_classifications.csv")
#load subjects
subjects <- read_csv("../data/products/subjects_master.csv")

#clean up subjects table - not everything there needs to be attached to classifications
subjects_tidy <- subjects %>%
  select(scene,
         classification_count,
         zooniverse_id,
         metadata.timestamp,
         upper_left_row,
         upper_left_col,
         tile_upper_left_x_UTM,
         tile_upper_left_y_UTM,
         tile_lower_right_x_UTM,
         tile_lower_right_y_UTM,
         tile_lower_left_x_UTM,
         tile_lower_left_y_UTM,
         tile_upper_right_x_UTM,
         tile_upper_right_y_UTM,
         tile_upper_left_x_LL,
         tile_upper_left_y_LL,
         tile_lower_left_x_LL,
         tile_lower_left_y_LL,
         tile_upper_right_x_LL,
         tile_upper_right_y_LL,
         tile_lower_right_x_LL,
         tile_lower_right_y_LL)

subjects_tidy <- subjects_tidy %>%
  mutate(DATE_ACQUIRED = as_date(metadata.timestamp))
sub_head <- head(subjects_tidy)


#get rid of old bad coordinates
classifications$upper_right_x <- NULL 
classifications $upper_right_y <- NULL
classifications$lower_left_x <- NULL
classifications$lower_left_y <- NULL

#join on good coordinates
classifications <- left_join(classifications, subjects_tidy, by = c("subject_zooniverse_id"="zooniverse_id"))
write_csv(classifications, "../data/products/classifications_master.csv")

