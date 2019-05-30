library(tidyverse)
library(sf)
library(fasterize)
library(raster)


#load and filter
classifications <- readRDS("../../data/output/raw_data_pipeline/level_0/zone_10_classifications.rds")
classifications$user_name <- as.factor(classifications$user_name) 

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
    mutate(replicate = rep(1:5, len = 60))
}

#given a subject, generate user combinations
user_combos <- create_user_combos(one_subject)

####stepwise workflow - need to convert to function
user_combos[20,]

#convert list element to vector of row numbers
#this is an arbitrary row, should go down list stepwise when automated
combo_vec <- unlist(user_combos[20,]$combination)
user_combos <- unite(user_combos, n_replicate, c("n", "replicate"), sep = "_", remove = FALSE)
#find the matching rows in classification data
combo_classifications <- one_subject %>%
  slice(combo_vec) 
#find n_replicate that matches
rep_info <- user_combos %>%
  filter(unlist(user_combos$combination) == combo_vec)

#rasterize these rows
r <- fasterize(combo_classifications,
          raster(one_subject, nrows = 400, ncols = 364), #and a template for the raster
          field = "user_name", #must be a factor (bug?)
          fun = "count")
#check it out
plot(r)
########

#now as a function



#given a subject, make combos
aggregate_user_combos <- function(combos_one_sub){
  #make a list of user combos
  combo_vec <- unlist(combos_one_sub)
  print(combo_vec)
  #find the matching rows in classification data
  combo_classifications <- one_subject %>%
    slice(combo_vec)
}


combined_geom_list <- map(user_combos[[3]], aggregate_user_combos)

#this actually works and creates a big list of all combinations for one scene.
#take this list and iterate the rasterization code across it
write_dir <- "../../data/output/raw_data_pipeline/level_1/min_combo_tiles/"

rasterize_user_combos <- function(one_combo){
  ID <- one_combo$subject_zooniverse_id[1]
  print(ID)
  
  rast <- fasterize(one_combo,
                    raster(one_combo, nrows = 400, ncols = 364),
                    field = "user_name",
                    fun = "count")
  
  filename <- str_c(write_dir, ID,  ".grd")
  #print(str_c("Writing subject ", ID, " to ", filename))
  writeRaster(rast, filename, overwrite = TRUE)
}

# this almost works. need to give each replicate a unique ID for file naming
#added ID fields to user combo table, need to carry through

test_map <- map(combined_geom_list, rasterize_user_combos)

#is this working?
test_rast <- test_map[[20]]

plot(combined_geom_list[[20]])
plot(test_map[[20]])

plot(st_as_sf(test_rast))
