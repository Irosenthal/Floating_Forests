#' ##############################
#' Separate classification, yes/no, 
#' and other workflow data into separate 
#' .Rds files
#' 
#' ##############################


#load libraries
library(raster)
library(tidyverse)
library(furrr)
library(readr)
library(sf)
library(spex)
library(fasterize)

#setup multicore environment to use all cores available ####
plan(multiprocess)
#plan(sequential) #for testing

#the relevant directories
read_dir <- "../../../data/relaunch_data/level_0/sf_tiles"
write_fig_dir <-  "../../../figures/relaunch_viz/"
write_data_dir <-  "../../../data/relaunch_data/level_0/"

files <- list.files(read_dir, full.names=TRUE)

#read in the files
sf_files <- map(files, readRDS)

#get utm zones
get_utm <- function(sf_obj){
  #get crs
  proj_crs <- st_crs(sf_obj)
  
  #get zone #
  str_extract(proj_crs$proj4string, "\\+zone=[0-9]+") %>%
    str_remove("\\+zone=") %>%
    as.numeric
}

utms <- map(sf_files, get_utm)

#make a utm list
utm_sf <- vector("list", length = length(unique(utms)))
names(utm_sf) <- unique(utms)

#add in data
utm_sf <- imap(utm_sf, ~sf_files[utms==.y])

#filter NA for now
utm_sf <- utm_sf[(names(utm_sf)!="NA")]

#combine pieces
merged_utms <- map(utm_sf,
                   ~reduce(.x, rbind))

saveRDS(merged_utms, str_c(write_data_dir, "merged_sf_tiles.Rds"))


ggplot(merged_utms[[1]] %>% filter(threshold>4)) +
  geom_sf(aes(fill = factor(threshold)))

ggsave(str_c(write_fig_dir, "threshold_4_utm_20.jpg"))


## reproject and rasterize

#read in falklands coastline
merged_utms <- readRDS(str_c(write_data_dir, "merged_sf_tiles.Rds"))

falklands <- st_read("../../../data/coastlines/falklands/Falklands.shp")
merged_utms_filter <- map(merged_utms, ~filter(.x, threshold>=4))

#fix s hemisphere landsat issue
#add 1e7 to y
merged_utms_filter_trans <- map(merged_utms_filter, ~.x %>% mutate(geometry = geometry+c(0,1e7)))

for(i in 1:length(merged_utms_filter_trans)){
  merged_utms_filter_trans[[i]] <- st_set_crs(merged_utms_filter_trans[[i]], st_crs(merged_utms_filter[[i]]))
}

merged_reproj <- map(merged_utms_filter_trans, st_transform, crs = st_crs(falklands))
merged_reproj <- reduce(merged_reproj, rbind)

#write out an intermediary....
save(merged_reproj, file = str_c(write_data_dir, "reproj_falklands.Rds"))

no_na_max <- function(x) max(x, na.rm=T)
bbox2extent <- function(x){
  b <- st_bbox(x)
  extent(c(b[1], b[3], b[2], b[4]))
}

raster_templ <- raster(bbox2extent(merged_reproj), 
                       crs = st_crs(merged_reproj),
                       res = c(0.000115, 9.01e-05))

combined_falklands_kelp <- fasterize(merged_reproj, raster = raster_templ, fun = no_na_max)

writeRaster(combined_falklands_kelp, str_c(write_dir, "merged_falkands.tif"))
