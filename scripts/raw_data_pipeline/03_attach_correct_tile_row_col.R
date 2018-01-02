library(dplyr)
library(readr)
library(tidyr)


#rotating row/col grid isn't trivial
rotate <- function(x) t(apply(x, 2, rev))


#make tile IDs
x<-c(1:20)
y<-c(1:20)
#each row col combo
mat_test <- expand.grid(x = x,y = y)


#each row/col combo gets a tile #. I will use this to assign these to actual FF data
tile <- c(1:nrow(mat_test))
mat_test <- cbind(mat_test, tile)
#convert this to a matrix and perform rotations on rows and cols
ID <- matrix(mat_test$tile, 20)
matr <- matrix(c(1:20), 20, 20)

true_tiles_rows <- matrix(c(1:20), 20, 20) 
true_tiles_cols <- t(true_tiles_rows)


ff_tiles_rows <- rotate(rotate(rotate(true_tiles_rows)))
ff_tiles_cols <-- rotate(rotate(rotate(true_tiles_cols)))

true_row_vec <- as.vector(true_tiles_rows)
true_col_vec <- as.vector(true_tiles_cols)
ff_row_vec <- rev(as.vector(ff_tiles_rows))
ff_col_vec <- rev(as.vector(abs(ff_tiles_cols)))

#create a dataframe matching up old/bad row cols with new/correct ones. this will be used to join correct row cols to subjects
grid_compare <- data.frame(bad_rows = true_row_vec, bad_cols = true_col_vec,  true_row = ff_row_vec,  true_col = ff_col_vec)
write.csv(grid_compare, "../data/grid_compare.csv")


#test it on one scene

subjects_complete <- read_csv("../data/products/subjects_with_scene_corners.csv")

scenelookup <- subjects_complete %>%
    filter(scene == "LE70440351999204EDC01")


correct_subjects_one_scene <- left_join(scenelookup, grid_compare, by = c("bad_rows", "bad_cols"))


#Looks good! proceed with full dataset
subjects_master <- left_join(subjects_complete, grid_compare, by = c("bad_rows", "bad_cols"))

write.csv(subjects_master, "../data/products/subjects_master.csv")

