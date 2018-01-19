library(tidyverse)
library(broom)
library(sp)
library(spdplyr)
library(ggplot2)
library(rgdal)
library(raster)
library(rgeos)
library(lubridate)


get_classifications <- function(filename = "ff_consensus_polys_zone_10.rds",
                                datadir = "../../data/output/consensus_shapefiles/",
                                return_value = "df"){

  #read in spatialPolygonsDataFrame (shapefile)
  #classifications <- readOGR("../../data/output/consensus_shapefiles/ff_polys_proj.sqlite", "ff_consensus", stringsAsFactors=FALSE) #slower
  classifications <- readRDS(paste0(datadir, filename))

  
  #add time information
  classifications <- classifications %>% 
    mutate(scene_timestamp = parse_date_time(scene_timestamp, orders="ymd"),
           quarter = quarter(scene_timestamp, with_year=TRUE, fiscal_start=11))
  
  if(return_value == "spdf") return(classifications)
  
  #convert to data frame
  classifications_df <- classifications %>% tidy
  classifications@data$id <- sapply(classifications@polygons, function(x) x@ID)
  classifications_df <- left_join(classifications_df, classifications@data)
  
  if(return_value == "df") return(classifications_df)
  
  return(list(spdf = classifications, df = classifications_df))
}