###############
#script to parse landsat scene corners from MTLs
#requires a folder with all MTLs 

library(tidyr)
library(dplyr)
library(readr)
library(stringr)
#create a table of all scenes in the folder
scenes <- as.data.frame(list.files("../data/all_MTLs"))
colnames(scenes) <- "scene"
scenes$scene <- as.character(scenes$scene)

#make a column with filename/path to feed into function
pre <- "../data/all_MTLs/"

#read in all data from all mtls
corner_parse <- function(scene){
    print(scene)
    coords <- as.data.frame(read.table(file = paste0(pre,scene), fill	
                                       = T))
    coords <- filter(coords, 
                     V1 == "CORNER_UL_LAT_PRODUCT"|
                         V1 == "CORNER_UL_LON_PRODUCT"|
                         V1 == "CORNER_UR_LAT_PRODUCT"|
                         V1 == "CORNER_UR_LON_PRODUCT"|
                         V1 == "CORNER_LL_LAT_PRODUCT"|
                         V1 == "CORNER_LL_LON_PRODUCT"|
                         V1 == "CORNER_LR_LAT_PRODUCT"|
                         V1 == "CORNER_LR_LON_PRODUCT"|
                         V1 == "CORNER_UL_PROJECTION_X_PRODUCT"|
                         V1 == "CORNER_UL_PROJECTION_Y_PRODUCT"|
                         V1 == "CORNER_UR_PROJECTION_X_PRODUCT"|
                         V1 == "CORNER_UR_PROJECTION_Y_PRODUCT"|
                         V1 == "CORNER_LL_PROJECTION_X_PRODUCT"|
                         V1 == "CORNER_LL_PROJECTION_Y_PRODUCT"|
                         V1 == "CORNER_LR_PROJECTION_X_PRODUCT"|
                         V1 == "CORNER_LR_PROJECTION_Y_PRODUCT"|
                         V1 == "PRODUCT_UL_CORNER_LAT"|
                         V1 == "PRODUCT_UL_CORNER_LON"|
                         V1 == "PRODUCT_UR_CORNER_LAT"|
                         V1 == "PRODUCT_UR_CORNER_LON"|
                         V1 == "PRODUCT_LL_CORNER_LAT"|
                         V1 == "PRODUCT_LL_CORNER_LON"|
                         V1 == "PRODUCT_LR_CORNER_LAT"| 
                         V1 == "PRODUCT_LR_CORNER_LON"|
                         V1 == "PRODUCT_UL_CORNER_MAPX"|
                         V1 == "PRODUCT_UL_CORNER_MAPY"|
                         V1 == "PRODUCT_UR_CORNER_MAPX"| 
                         V1 == "PRODUCT_UR_CORNER_MAPY"|
                         V1 == "PRODUCT_LL_CORNER_MAPX"| 
                         V1 == "PRODUCT_LL_CORNER_MAPY"| 
                         V1 == "PRODUCT_LR_CORNER_MAPX"| 
                         V1 == "PRODUCT_LR_CORNER_MAPY"|
                         V1 == "UTM_ZONE"|
                         V1 == "ZONE_NUMBER"|
                         V1 == "DATE_ACQUIRED"|
                         V1 == "ACQUISITION_DATE"|
                         V1 == "SPACECRAFT_ID"|
                         V1 == "WRS_ROW"|
                         V1 == "ENDING_ROW"|
                         V1 == "WRS_PATH")
    coords <- coords[,3]
}
######
#run it on all rows and rename columns to be meaningful
parsed_coords <- as.data.frame(lapply(scenes$scene, corner_parse))
parsed_coords <- as.data.frame(t(parsed_coords))
colnames(parsed_coords) <- c("SPACECRAFT_ID",
                             "WRS_PATH",
                             "WRS_ROW",
                             "DATE_ACQUIRED",
                             "CORNER_UL_LAT_PRODUCT",
                             "CORNER_UL_LON_PRODUCT",
                             "CORNER_UR_LAT_PRODUCT",
                             "CORNER_UR_LON_PRODUCT",
                             "CORNER_LL_LAT_PRODUCT",
                             "CORNER_LL_LON_PRODUCT",
                             "CORNER_LR_LAT_PRODUCT",
                             "CORNER_LR_LON_PRODUCT",
                             "CORNER_UL_PROJECTION_X_PRODUCT",
                             "CORNER_UL_PROJECTION_Y_PRODUCT",
                             "CORNER_UR_PROJECTION_X_PRODUCT",
                             "CORNER_UR_PROJECTION_Y_PRODUCT",
                             "CORNER_LL_PROJECTION_X_PRODUCT",
                             "CORNER_LL_PROJECTION_Y_PRODUCT",
                             "CORNER_LR_PROJECTION_X_PRODUCT",
                             "CORNER_LR_PROJECTION_Y_PRODUCT",
                             "UTM_ZONE")


correct_metadata_from_MTLs <- cbind(scenes, parsed_coords)



#clean up scene column to get rid of _MTL.txt
#this might look ugly but is just cleaning up and removing the old scene column
scenes_trimmed <- as.data.frame(str_sub(correct_metadata_from_MTLs$scene, end = -9))
correct_metadata_from_MTLs <- cbind(scenes_trimmed, correct_metadata_from_MTLs)
colnames(correct_metadata_from_MTLs)[1] <- "scene_trimmed"
correct_metadata_from_MTLs <- correct_metadata_from_MTLs %>%
    dplyr::select(-scene)
colnames(correct_metadata_from_MTLs)[1] <- "scene"

correct_metadata_from_MTLs <- correct_metadata_from_MTLs %>%
    tidyr::unite("path_row", c(WRS_PATH, WRS_ROW), remove = FALSE )

write_csv(correct_metadata_from_MTLs, "../data/products/scene_metadata.csv")
write_csv(correct_metadata_from_MTLs, "../data/products/scene_metadata.csv")
#attach these to the classifications
classifications <- read_csv("../data/prouducts")
