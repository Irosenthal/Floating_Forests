library(tidyverse)
library(sf)
library(fasterize)
library(raster)


#load and filter
classifications <- readRDS("../../data/output/raw_data_pipeline/level_0/zone_10_classifications.rds")
classifications$user_name <- as.factor(classifications$user_name) 

#to run on whole dataset




#filter to one tile for testing purposes
one_subject <- classifications %>%
  filter(subject_zooniverse_id == "AKP00016e6")


#function to generate user combos
create_user_combos <- function(sf_data){
  n <- rep(1:nrow(sf_data), each = 5)
  user_combos <- data.frame(n = n)
  user_combos <- user_combos %>%
    rowwise() %>%
    #this is gross, needed it because matrices are weird
    mutate(combination = list(sample(as.data.frame(row(sf_data))$V1, n, replace = FALSE))) %>%
    ungroup() %>%
    as_tibble() %>%
    #add a column for replicate number. 
    #downstream, n and replicate columns will be used for naming raster outputs
    mutate(replicate = rep(1:5, len = nrow(sf_data)*5))
}

 #given a subject, generate user combinations
user_combos <- create_user_combos(one_subject)



#user_combos <- map(one_subject, create_user_combos)

#given a set of combinations
aggregate_user_combos <- function(n, combination, replicate, subject_zooniverse_id){
  #make a list of user combos
  combo_vec <- unlist(combination)
  
  #print(combo_vec)
  ID <- subject_zooniverse_id
  print(str_c("Aggregating subject ", ID, ", threshold: ", n, ", replicate: ", replicate))
  
    #find the matching rows in classification data
  #subject_classifiations: classifications filtered to 1 sub
  combo_classifications <- classifications %>%
    filter(subject_zooniverse_id == ID) %>%
    slice(combo_vec) %>%
    #add n and rep to data
    mutate(n = n[1], rep = replicate[1])
  combo_classifications
}

combined_geom_list <- pmap(user_combos, aggregate_user_combos) 



#take this list and iterate the rasterization code across it

#set write out directory
write_dir <- "../../data/output/raw_data_pipeline/level_1/min_combo_tiles/"

one_combo <- combined_geom_list[2]
rasterize_user_combos <- function(one_combo){
  ID <- one_combo$subject_zooniverse_id[1]
  n <- one_combo$n[1]
  replicate <- one_combo$rep[1]
  print(str_c("Rasterizing subject ", ID, ", threshold: ", n, ", replicate: ", replicate))
  rast <- fasterize(one_combo,
                    raster(one_combo, nrows = 400, ncols = 364),
                    field = "user_name",
                    fun = "count")
  
  filename <- str_c(write_dir, ID, "_", n, "_", replicate,  ".grd")
  #print(str_c("Writing subject ", ID, " to ", filename))
  writeRaster(rast, filename, overwrite = TRUE)
}

# this almost works. need to give each replicate a unique ID for file naming

test_map <- map(combined_geom_list, rasterize_user_combos)



#is this working?
test_rast <- test_map[[20]]

plot(combined_geom_list[[10]])
plot(test_map[[40]])

plot(st_as_sf(test_rast))




############
#full run

#take a subject
#make unique IDs
#subset classifications
#write out raster

#for testing, otherwise just split all classifications instead of "two_subjects"
subject_list <- split(two_subjects, two_subjects$subject_zooniverse_id)
#generate user combinations and add zooniverse id as a column for later bookkeeping
user_combos <- map(subject_list, create_user_combos)
user_combos <-  mapply(`[<-`, user_combos, 'subject_zooniverse_id',
                         value = names(user_combos), SIMPLIFY = FALSE)
user_combos <- dplyr::bind_rows(user_combos)
#subset classifications for each combo  
combined_geom_list <- pmap(user_combos, aggregate_user_combos) 
#rasterize and write out  
rast_out <- map(combined_geom_list, rasterize_user_combos)
  


