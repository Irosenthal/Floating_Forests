#####attach corners from nested scene list to the subjects
library(dplyr)
library(purrr)
library(readr)
#get nested corner/scene list
grid_list <- readRDS("../data/products/all_scenes_list")
#get subjects
raw_subjects <- read_csv("../data/products/subjects_master.csv")


#test with just one scene first
scene_test <- subjects %>%
    filter(scene == "LE70440351999204EDC01")

#scene_out_test, made in 04_calculate_tile_corners takes the place of grid_list for the single scene.
grid_list <- scene_out_test

#expand out this list into a dataframe for joining
current_grid <- grid_list %>%
    purrr::map_df(`[`,c(1:7)) 

#add lower right row/cols
#in final version do all 4 corners
scene_test <- scene_test %>%
    rename(upper_left_col = true_col) %>%
    rename(upper_left_row = true_row) %>%
    mutate(lower_right_col = upper_left_col + 1) %>%
    mutate(lower_right_row = upper_left_row + 1)
    
#create a new df for x and y - these are what will be joined back on to subjects table to find each corner
filtered_x <- current_grid %>%
    select(scene, longridindex.x, longridindex.Col)
filtered_y <-current_grid %>%
    select(scene, latgridindex.y, latgridindex.Row)

#now the joins
scene_test <- left_join(scene_test, filtered_x, by = c("scene", "upper_left_col" = "longridindex.Col"))
scene_test <- left_join(scene_test, filtered_y, by = c("scene", "upper_left_row" = "latgridindex.Row"))


scene_test <- left_join(scene_test, filtered_x, by = c("scene", "lower_right_col" = "longridindex.Col"))
scene_test <- left_join(scene_test, filtered_y, by = c("scene", "lower_right_row" = "latgridindex.Row"))


scene_test <- scene_test %>%
    rename(tile_upper_left_x_UTM = longridindex.x.x) %>%
    rename(tile_upper_left_y_UTM = latgridindex.y.x ) %>%
    rename(tile_lower_right_x_UTM = longridindex.x.y) %>%
    rename(tile_lower_right_y_UTM = latgridindex.y.y)

#that worked for one scene. below is the same code structured to run on entire dataset.

#add other corners
subjects <- raw_subjects %>%
  rename(upper_left_col = true_col) %>%
  rename(upper_left_row = true_row) %>%
  mutate(lower_left_col = upper_left_col) %>%
  mutate(lower_left_row = upper_left_row +1 ) %>%
  mutate(upper_right_col = upper_left_col +1) %>%
  mutate(upper_right_row = upper_left_row) %>%
  mutate(lower_right_col = upper_left_col + 1) %>%
  mutate(lower_right_row = upper_left_row + 1)
  

#create a new df for x and y - these are what will be joined back on to subjects table to find each corner
filtered_x <- current_grid %>%
  select(scene, longridindex.x, longridindex.Col)

filtered_y <-current_grid %>%
  select(scene, latgridindex.y, latgridindex.Row)

#now the joins

#upper left first
subjects <- left_join(subjects, filtered_x, by = c("scene", "upper_left_col" = "longridindex.Col"))
subjects <- left_join(subjects, filtered_y, by = c("scene", "upper_left_row" = "latgridindex.Row"))

#clean up col names as we go
subjects <- subjects %>%
  rename(tile_upper_left_x_UTM = longridindex.x) %>%
  rename(tile_upper_left_y_UTM = latgridindex.y)

#then lower right
subjects <- left_join(subjects, filtered_x, by = c("scene", "lower_right_col" = "longridindex.Col"))
subjects <- left_join(subjects, filtered_y, by = c("scene", "lower_right_row" = "latgridindex.Row"))

#clean up col names as we go
subjects <- subjects %>%
  rename(tile_lower_right_x_UTM = longridindex.x) %>%
  rename(tile_lower_right_y_UTM = latgridindex.y)

#then lower left
subjects <- left_join(subjects, filtered_x, by = c("scene", "lower_left_col" = "longridindex.Col"))
subjects <- left_join(subjects, filtered_y, by = c("scene", "lower_left_row" = "latgridindex.Row"))

#clean up col names as we go
subjects <- subjects %>%
  rename(tile_lower_left_x_UTM = longridindex.x) %>%
  rename(tile_lower_left_y_UTM = latgridindex.y)

#then upper right
subjects <- left_join(subjects, filtered_x, by = c("scene", "upper_right_col" = "longridindex.Col"))
subjects <- left_join(subjects, filtered_y, by = c("scene", "upper_right_row" = "latgridindex.Row"))

#clean up col names as we go
subjects <- subjects %>%
  rename(tile_upper_right_x_UTM = longridindex.x) %>%
  rename(tile_upper_right_y_UTM = latgridindex.y)

write_csv(subjects, "../data/products/subjects_master.csv")
