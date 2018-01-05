library(tidyverse)
library(broom)
library(sp)
library(spdplyr)
library(ggplot2)
library(gganimate)
library(ggmap)
library(rgdal)
library(raster)
library(rgeos)
library(lubridate)

#read in spatialPolygonsDataFrame (shapefile)
#classifications <- readOGR("../../data/output/consensus_shapefiles/ff_polys_proj.sqlite", "ff_consensus", stringsAsFactors=FALSE) #slower
classifications <- readRDS("../../data/output/consensus_shapefiles/ff_polys_proj.rds")

#add time information
classifications <- classifications %>% 
  mutate(scene_timestamp = parse_date_time(scene_timestamp, orders="ymdHMS"),
         quarter = quarter(scene_timestamp, with_year=TRUE, fiscal_start=11))

#convert to data frame
classifications_df <- classifications %>% tidy
classifications@data$id <- sapply(classifications@polygons, function(x) x@ID)
classifications_df <- left_join(classifications_df, classifications@data)
