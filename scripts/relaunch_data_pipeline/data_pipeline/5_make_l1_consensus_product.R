#' ##############################
#' Make aggregated sf objects of the
#' 
#' ##############################


#'--------------------------
# Prep ####
#'--------------------------

#load libraries
library(tidyverse)
library(furrr)
library(readr)
library(sf)
library(spex)
library(fasterize)

#setup multicore environment to use all cores available
plan(multiprocess)
#plan(sequential) #for testing

#the relevant directories
raw_data_dir <- "../../../data/relaunch_data/raw_data/"
read_dir <- "../../../data/relaunch_data/level_0/"
write_fig_dir <-  "../../../figures/relaunch_viz/"
write_data_dir <-  "../../../data/relaunch_data/level_1/"

sf_files <- list.files(str_c(read_dir, "sf_tiles/"), full.names=TRUE)
ff_relaunch_subjects <- readRDS(str_c(read_dir, "ff_relaunch_subjects.rds")) %>%
  mutate(acquired_date = parse_date(acquired_date, format = "%Y-%m-%d"), #problem with some dates - diff format
         Year = lubridate::year(acquired_date),
         Season = lubridate::quarter(acquired_date, with_year=T, fiscal_start=12))

#a pipeline for fixing sf tiles

fix_landsat <- function(a_sf){
  a_sf %>%
  mutate(geometry = geometry+c(0,1e7)) %>%
  st_set_crs(st_crs(a_sf))
}

#'--------------------------
# Read and merge tiles ####
#'--------------------------

#read in the files and fix their southern hemisphere issues
sf_tiles <- future_map(sf_files, ~readRDS(.x) %>% filter(threshold>3) %>% fix_landsat())


#get utm zones
get_utm <- function(sf_obj){
  #get crs
  proj_crs <- st_crs(sf_obj)
  
  #get zone #
  str_extract(proj_crs$proj4string, "\\+zone=[0-9]+") %>%
    str_remove("\\+zone=") %>%
    as.numeric
}

utms <- map(sf_tiles, get_utm)

#make a utm list
utm_sf <- vector("list", length = length(unique(utms)))
names(utm_sf) <- unique(utms)

#add in data
utm_sf <- imap(utm_sf, ~sf_tiles[utms==.y])

#filter NA for now
utm_sf <- utm_sf[(names(utm_sf)!="NA")]

#combine pieces
merged_utms <- future_map(utm_sf,
                   ~reduce(.x, rbind))

#save for future use
saveRDS(merged_utms, str_c(write_data_dir, "merged_sf_tiles.Rds"))


#'----------------------------------
# Make merged latlong shapefile ####
#'----------------------------------

#load in the falklands shapefile
f <- st_read("../../../data/coastlines/falklands/Falklands.shp")

#reproject the utm zones to the same projection as the falklands shapefile (lat/long wgs44)
latlong_sf <- future_map(merged_utms, st_transform, st_crs(f))

#merge it all!
merged_sf_latlong <- reduce(latlong_sf, rbind)

#crop it!
merged_sf_latlong <- st_difference(merged_sf_latlong, f)

#save for future use
saveRDS(merged_sf_latlong, str_c(write_data_dir, "merged_sf_latlong.Rds"))

#save as a shapefile

#' ----------------------------------------
# Make merged shapefiles by threshold ####
#' ----------------------------------------
all_kelp_sf <- merged_sf_latlong %>%
  group_by(threshold) %>%
  summarize()

saveRDS(all_kelp_sf, str_c(write_data_dir, "all_kelp_sf.Rds"))

#' -------------------------------------------------
# Make merged shapefiles by year, and threshold ####
#' -------------------------------------------------
merged_sf_latlong_dates <- merged_sf_latlong %>% 
  mutate(subject_id = as.integer(subject_id)) %>%
  left_join(ff_relaunch_subjects %>% select(subject_id, Year, Season))

all_annual_kelp_sf <- merged_sf_latlong_dates %>%
  group_by(threshold, Year) %>%
  summarize()

saveRDS(all_annual_kelp_sf, str_c(write_data_dir, "all_annual_kelp_sf.Rds"))
#all_annual_kelp_sf$area <- st_area(all_annual_kelp_sf)
#ggplot(all_annual_kelp_sf, aes(x = Year, y = as.numeric(area), color = factor(threshold))) + geom_line()


#' ---------------------------------------------------------
# Make merged shapefiles by season, year, and threshold ####
#' ---------------------------------------------------------
all_annual_kelp_seasonal <- merged_sf_latlong_dates %>%
  group_by(threshold, Season) %>%
  summarize(Year = max(Year, na.rm=T))

saveRDS(all_annual_kelp_seasonal, str_c(write_data_dir, "all_annual_kelp_seasonal.Rds"))

#all_annual_kelp_seasonal$area <- st_area(all_annual_kelp_seasonal)
#ggplot(all_annual_kelp_seasonal, aes(x = Season, y = as.numeric(area), color = factor(threshold))) + geom_line()


#' ---------------------------------------------------------
# Make merged shapefiles by decade and threshold ####
#' ---------------------------------------------------------
all_annual_kelp_decadal<- merged_sf_latlong_dates %>%
  mutate(Decade = floor(Year/10)*10) %>%
  group_by(threshold, Decade) %>%
  summarize()

#all_annual_kelp_decadal$area <- st_area(all_annual_kelp_decadal)
#ggplot(all_annual_kelp_decadal, aes(x = Decade, y = area, color = factor(threshold))) + geom_line()

saveRDS(all_annual_kelp_seasonal, str_c(write_data_dir, "all_annual_kelp_decadal.Rds"))


#' ---------------------------------------------------------
# Make merged shapefiles by season and threshold, not year ####
#' ---------------------------------------------------------
all_annual_kelp_by_season <- merged_sf_latlong_dates %>%
  mutate(Season = str_replace(Season, "([0-9]+\\.)([1-4])$", "\\2"),
         Season = fct_recode(factor(Season), 
       #                      Winter = "1", Spring = "2", Summer = "3", Fall="4")) %>%
       `Austral Summer` = "1", `Austral Fall` = "2", `Austral Winter` = "3", `Austral Spring`="4")) %>%
  group_by(threshold, Season) %>%
  summarize()

saveRDS(all_annual_kelp_by_season, str_c(write_data_dir, "all_annual_kelp_by_season.Rds"))
# 
# 
library(ggplot2)

jpeg(str_c(write_fig_dir, "season_test.jpg"), width = 1024, height = 768)
ggplot(all_annual_kelp_by_season %>% filter(threshold==6)) +
  geom_sf() +
  facet_wrap(~Season) +
  theme_map(base_size = 18)
dev.off()
# 
# #project a_file to map
# #xmin: -58.32532 ymin: -51.41614 xmax: -58.19128 ymax: -51.36322
# 
# # xmin: -58.32532 ymin: -51.41614 xmax: -58.19128 ymax: -51.38297
# f_patch <- st_crop(f, merged_sf_latlong)
# 
# #plot them together to check
# plot(f_patch["area"], axes=T, key.pos = NULL, reset = FALSE,
#      xlim = c(-58.32532, -58.19128), ylim = c( -51.41614, -51.36322), col = "black")
# plot(patch["threshold"], axes=T, key.pos = NULL, reset = FALSE,
#      xlim = c(-58.32532, -58.19128), ylim = c( -51.41614, -51.36322), add=TRUE)
# 
#test with leaflet
library(leaflet)

pal <- colorNumeric(
  palette = "YlGnBu",
  domain = merged_sf_latlong$threshold
)

leaflet(all_kelp_sf %>% filter(threshold==7)) %>%
  #  addTiles() %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  addPolygons(fillColor = ~pal(threshold),
              color = ~pal(threshold),
              opacity = 1.0, fillOpacity = 1.0)%>%
  addLegend("bottomright", pal =pal, values = ~threshold,
            title = "# of Users",
            opacity = 1
  )

# 
# 
