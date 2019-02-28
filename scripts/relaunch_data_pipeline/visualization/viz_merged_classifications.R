#' ##############################
#' Separate classification, yes/no, 
#' and other workflow data into separate 
#' .Rds files
#' 
#' ##############################


#load libraries
library(tidyverse)
library(furrr)
library(readr)
library(sf)

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
