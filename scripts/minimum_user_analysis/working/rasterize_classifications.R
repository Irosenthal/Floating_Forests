library(sf)
library(fasterize)
library(tidyverse)
library()


#function to take a pile of SFs and convert them to rasters

#data
classifications <- readRDS("../../data/output/raw_data_pipeline/level_0/zone_10_classifications.rds")

#basic rasterization
#make user_name a factor to be used with fasterize
classifications$user_name <- as.factor(classifications$user_name) 
#filter to one subject for testing
one_subject <- classifications %>%
  filter(subject_zooniverse_id == "AKP000051z")
  

#last thought of friday: some of these are multipolygons with only one  point repeated many times. need to build in some sort of error catcher into function.

r <- fasterize(one_subject, #give it an SF
               raster(one_subject, nrows = 400, ncols = 364), #and a template for the raster
               field = "user_name", #must be a factor (bug?)
               fun = "count")
plot(r)





bad_sub
