library(tidyverse)

#load and filter
classifications <- read_rds("../../data/output/raw_data_pipeline/sp_classifications_df_zone_10.rds")
library(sf)
#filter to tile we care about

spdf_one_tile <- classifications$SPDF$AKP00016e6
spdf_one_tile_data <- spdf_one_tile@data
spdf_one_tile_polys <- spdf_one_tile@polygons
spdf_one_tile_df <- as.data.frame(spdf_one_tile)

##### sf conversion 
#this works - just need to go utm > ll > sf
spdf_one_tile_ll <- spTransform(spdf_one_tile, CRS("+proj=longlat +datum=WGS84"))
one_tile_sf = st_as_sf(spdf_one_tile_ll, "sf")

#100  numbers between 1 and 15
#n <- round(runif(100, 1, 12)) #random
n <- rep(1:nrow(one_tile_sf), each = 5)
#users <- letters[1:12] #fake users
#value <- round(runif(15, 1, 10))

#now pick 100 combos of users, n random ones each
#to make a vector of n random users

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

user_combos <- create_user_combos_row(one_tile_sf)

#to link with polys, do an if in statement?







#
group_by
summarise(geometry = st_union(geometry)) 
  
#do I even want st_union? I think I Just want to make a multipolygon?






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





