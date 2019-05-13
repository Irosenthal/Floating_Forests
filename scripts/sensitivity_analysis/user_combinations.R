library(tidyverse)

#load and filter
classifications <- read_rds("../../data/output/raw_data_pipeline/sp_classifications_df_zone_10.rds")
library(sf)
library(tidyverse)


#filter to one tile and convert to sf

one_tile_sf <- classifications$SPDF$AKP00016e6 %>%
  #go from UTM to LL
  spTransform(CRS("+proj=longlat +datum=WGS84")) %>%
  #convert to sf
  st_as_sf(classifications, "sf")

#clean up environment
rm(classifications)


#100 numbers between 1 and 15
#n <- round(runif(100, 1, 12)) #random
n <- rep(1:nrow(one_tile_sf), each = 5) #uniform

#function to generate user combos, given an sf object
#outputs number of users in combination as well as row numbers of each randomly selected user

create_user_combos <- function(sf_data){
  n <- rep(1:nrow(sf_data), each = 5)
  user_combos <- data.frame(n = n)
  user_combos <- user_combos %>%
    rowwise() %>%
    mutate(combination = list(sample(row(sf_data), n, replace = FALSE))) %>%
    ungroup() %>%
    as_tibble()
}

user_combos <- create_user_combos(one_tile_sf)
user_combos$combination <- as.numeric()



#find polygons based on these lists
#unique group identifier?
#mutate a new column?

#have it apply a filter to main sf and then spit out an sf for each combo?
#mutate(new_col = oldsf[user_rows,])





#convert to a raster - will need for MCC
#make user_name a factor to be used with fasterize
one_tile_sf$user_name <- as.factor(one_tile_sf$user_name) 


r <- fasterize(one_tile_sf, #give it an SF
               raster(one_tile_sf, nrows = 400, ncols = 364), #and a template for the raster
               field = "user_name", #must be a factor (bug?)
               fun = "count")
plot(r)

#check if FF_methods functions still work
getKelpPixelsFromRaster(r)
getKelpPixelsFromRaster(r, 2)
getKelpPixelsFromRaster(r, 12)
getKelpPixelsFromRaster(r, 13)
#they do





