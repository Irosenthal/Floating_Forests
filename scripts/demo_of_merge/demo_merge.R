###########
# Libraries
###########

library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(lubridate)
library(tidyverse)
library(spdplyr)

a_spdf <- readRDS("./subject_demo.rd")

#----------------
#For one subject
#----------------

# Make a blank raster layer
combnR <- raster(crs=a_spdf@proj4string, ext=extent(a_spdf))

# Get the consensus raster
arast <- rasterize(SpatialPolygons(a_spdf@polygons), combnR, 
                            fun="count")  

# What are the user thresholds possible in this subject?
thresholds <- na.exclude(unique(raster::values(arast)))

# Make a list of spdfs, 1 for each threshold
# This takes... a long time.
spdf_list <- lapply(thresholds, function(x){
  temp_rast <- arast
  raster::values(temp_rast)[raster::values(temp_rast)<x] <- NA
  raster::values(temp_rast)[!is.na(raster::values(temp_rast))] <- x
  rasterToPolygons(temp_rast, dissolve = TRUE)
})

# Now make it all into one spdf
merged_spdf <- do.call(rbind, spdf_list) %>%
  arrange(layer)

# Plot them
par(mfrow=c(1,2))
plot(arast)
plot(merged_spdf, col=merged_spdf@data$layer) #yuk - ggplot better
par(mfrow=c(1,1))

#--------------
# a spatio-temporal
# data set
#--------------

mont <- readRDS("./monterey_demo.rd")
