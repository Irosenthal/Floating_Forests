########
#script to attach scene metadata to subjects table. 
#This is done in order to streamline later scene slicing
#read in raw subject table and strip off bad info
#NOTE: LEAVE BAD ROW/COLS - NEEDED FOR CORRECTION


#Pull in metadata created in 01_parse_MTL.R
scene_metadata <- read_csv( "../data/products/scene_metadata.csv")

#pull in raw subjects table
subjects <- read_csv("../data/mongosubjectsfix.csv")
#clean up scene coluimn for join
subject_scenes_trimmed <- as.data.frame(str_sub(subjects$metadata.file , end = -8))
subjects <- cbind(subject_scenes_trimmed, subjects)
colnames(subjects)[1] <- "scene"
subjects <- subjects %>%
    select(-metadata.file)


#make a new set of columns that are complete row/cols
#must be done this way due to multiple incomplete columns in metadata
#these will be joined with my lookup table to stick on the correct row/col, and then get the right metadata corners stuck on

subjects <- subjects %>%
    mutate("bad_row"= paste0(metadata.row_no, metadata.row))
subjects <- subjects %>%
    mutate("bad_col"= paste0(metadata.col_no, metadata.col))
subjects <- subjects %>%
    mutate(bad_rows = gsub("NA", "", bad_row))
subjects <- subjects %>%
    mutate(bad_cols = gsub("NA", "", bad_col))
subjects$bad_rows <- as.numeric(subjects$bad_rows) +1
subjects$bad_cols <- as.numeric(subjects$bad_cols) +1
subjects <- subjects %>%
    select(-bad_row, -bad_col)
subjects <- subjects %>%
    unite("row_col",c(bad_rows, bad_cols), remove = FALSE)
#clean up subjects to free up RAM
subjects <- subjects %>%
    dplyr::select(-metadata.source_rows, -metadata.source_cols, -random, 
                  -project_id, -workflow_ids, -metadata.marking_count, 
                  -metadata.upper_right1, -metadata.upper_right2,
                  metadata.lower_left1, metadata.lower_left2, 
                  -metadata.row, -metadata.col)


#now join scene corners on to this df. take out everything not relevant join by scene

subjects_complete <- left_join(subjects, scene_metadata, by = "scene")



#spot check one of these against the MTL
scenelookup <- subjects_complete %>%
    filter(scene == "LE70440351999204EDC01")


write.csv(subjects_complete, "../data/products/subjects_with_scene_corners.csv")
