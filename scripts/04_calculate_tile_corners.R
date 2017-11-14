#code to take master scene file and calculate tile corners for each subject.
#after running this script, join with subjects table by scene, row, and column
#takes a very long time to run
library(dplyr)
library(readr)

scene_nest <- function(arow){ 
    LLX <- arow$CORNER_LL_PROJECTION_X_PRODUCT
    LRX <- arow$CORNER_LR_PROJECTION_X_PRODUCT
    LLY <- arow$CORNER_LL_PROJECTION_Y_PRODUCT
    ULY <- arow$CORNER_UL_PROJECTION_Y_PRODUCT
    
    #LLX <- unlist(test_subjects[arow,'CORNER_LL_PROJECTION_X_PRODUCT'])
    #LRX <- unlist(test_subjects[arow,'CORNER_LR_PROJECTION_X_PRODUCT'])
    #LLY <- unlist(test_subjects[arow,'CORNER_LL_PROJECTION_Y_PRODUCT'])
    #ULY <- unlist(test_subjects[arow,'CORNER_UL_PROJECTION_Y_PRODUCT'])
    
    lonlatpix <-expand.grid(x = seq(as.numeric(LLX), as.numeric(LRX), by = 30),  
                            y = seq(as.numeric(ULY), as.numeric(LLY), by = -30))
    #this assigns each to a unique  pixel to each set of utm coords within the scene
    lonlatpixIndex <- mutate(lonlatpix, PID = 1:nrow(lonlatpix))
    sizey <- n_distinct(lonlatpix$y)
    sizex <- n_distinct(lonlatpix$x) 
    
    #generate pixel numbers for corners of of each ff image
    height <- seq(1, sizey, by = 363)
    height <- height[1:20]
    width <- seq(1, sizex, by = 399)
    width <- width[1:20]
    
    #pull out individual pixels
    #these are the same as lat and lon in matlab script and make same numbers
    latpix <- lonlatpixIndex %>%
        distinct(y) %>%
        dplyr::select(y) %>%
        mutate(PID = c(1:n_distinct(y)))
    
    lonpix <- lonlatpixIndex %>%
        distinct(x) %>%
        dplyr::select(x) %>%
        mutate(PID = c(1:n_distinct(x)))
    
    #these are at least the first part to the lat and lonslices in matlab. 
    #they are at least labeling correctly and calling out the correct positions.
    #select the pixels that occur on tile corners and assign them a pixel ID
    latgridindex <- latpix[seq(1,length(latpix$y), by = 363),] %>%
        mutate(Row = c(1:n_distinct(y)))
    latgridindex <- latgridindex[1:20,]
    
    longridindex <- lonpix[seq(1,length(lonpix$x), by = 399),] %>%
        mutate(Col = c(1:n_distinct(x)))
    longridindex <- longridindex[1:20,]
    
    #need to account for 20th row due to incorrect data processing - rows and cols started at 0.
    
    lastlon <- lonpix[nrow(lonpix),]
    Col <- nrow(longridindex) +1 
    lonbind <- cbind(lastlon, Col)
    longridindex <- rbind(longridindex,lonbind)
    
    
    lastlat <- latpix[nrow(latpix),]
    Row <- nrow(latgridindex) +1 
    latbind <- cbind(lastlat, Row)
    latgridindex <- rbind(latgridindex,latbind)
    
    scene_index <- data.frame(longridindex = longridindex, latgridindex = latgridindex)
    
    #This write a csv for each scene - use for troubleshooting
    write.csv(scene_index, paste0("../data/scene_indices/", arow$scene_trimmed,".csv"), row.names=F)
    
    return(scene_index)
}

####################
#Test on one scene first
###################
######test set
scene_test <- scene_metadata %>%
    filter(scene == "LE70440351999204EDC01")
#reduce scenes for testing
scene_list <- data.frame(scene = rep(scene_test$scene, each = 21)) #populate a data.frame for an rbind

scene_out_test<- scene_test %>% 
    rowwise() %>%
    do(scene_nest(.))

scene_out_test <- cbind(scene_list, scene_out_test)
scene_out_test <- split(scene_out_test, f = scene_out_test$scene)
scene_out_test_df <- as.data.frame(scene_out_test)

#gets it close - within 15 meters. I believe this to be the difference between the corner and center of a pixel. 


#############run on all scenes###############
scenes <- read_csv("../data/products/scene_metadata.csv")

scene_list <- data.frame(scene = rep(scenes$scene, each = 21)) #populate a data.frame for an rbind
scene_out<- scenes %>% 
    rowwise() %>%
    do((scene_nest(.)))

#save this raw output just in case - not used going forward but skips the painful part 
saveRDS(scene_out, "../data/products/all_scenes.rds")

scene_out_bind <- cbind(scene_list, scene_out)
scene_out_list <- split(scene_out_bind, f = scene_out_bind$scene)
saveRDS(scene_out_list, "../data/products/all_scenes_list")


