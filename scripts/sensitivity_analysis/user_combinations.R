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


#take that list of user combos and map some function across them
#this works and i don't know why
map_dbl(users, ~sum(mydf$num[mydf$users %in% .x]))

