#' ####################################
#' Process where FF has been sampling
#' in order to create better consensus
#' ####################################

library(tidyverse)
library(sf) 
library(ggplot2)



source("./_pipeline_functions.R")
f <- st_read("../../../data/coastlines/falklands/Falklands.shp")

read_dir <-  "../../../data/relaunch_data/"
write_dir <- "../../../data/relaunch_data/level_0/"

landsat_fix <- function(x) x + 1e07

rejects <- list.files(str_c(read_dir, "raw_data/upload_manifests/"), 
                      pattern = "rejected",
                      full.names=TRUE)

read_reject <- function(afile){
  a_reject <- read_csv(afile) 
  
  a_reject %>%
    mutate(acquired_date = as.character(acquired_date)) %>%
    rename(subject_id = `#filename`, retirement_reason = `#reason`)%>%
    select(subject_id,  sensor_id, `#tile_UL_x`, `#tile_LR_x`, 
           `#tile_UL_y`,`#tile_LR_y`,
           `#utm_zone`,`!scene`,
           acquired_date,  retirement_reason) 
}

ff_relaunch_subjects <- readRDS(str_c(read_dir, "level_0/ff_relaunch_subjects.rds")) %>%
  mutate(subject_id = as.character(subject_id)) %>%
  select(subject_id,  sensor_id, `#tile_UL_x`, `#tile_LR_x`, 
         `#tile_UL_y`,  `#tile_LR_y`,
         `#utm_zone`,`!scene`,
         acquired_date,  retirement_reason) %>% 
  mutate_at(vars(contains("utm_zone")), as.numeric) %>%
  mutate_at(vars(contains("_y")), as.numeric) %>%
  mutate_at(vars(contains("_x")), as.numeric)
  
#bring observed and rejected tiles together
all_subj <- reduce(c(list(f1 = ff_relaunch_subjects), map(rejects, read_reject)),
                   bind_rows)

all_subj <-  all_subj %>%
  filter(`#utm_zone` != 19) %>%
  filter(!is.na(`#tile_UL_x`)) %>%
  filter(!is.na(`#tile_LR_y`)) %>%
  #mutate(str_replace(acquired_date, "\\-", "\\/")) %>%
  mutate(acquired_date = parse_date(acquired_date, format = "%Y-%m-%d"), #problem with some dates - diff format
         Year = lubridate::year(acquired_date),
         Season = lubridate::quarter(acquired_date, with_year=T, fiscal_start=12)) %>%
   mutate_at(vars(contains("_y")), landsat_fix) %>%
  filter(!is.na(`#tile_UL_x`))  %>%
  mutate(Season = str_replace(Season, "([0-9]+\\.)([1-4])$", "\\2"),
         Season = fct_recode(factor(Season), 
                             #                      Winter = "1", Spring = "2", Summer = "3", Fall="4")) %>%
                             `Austral Summer` = "1", `Austral Fall` = "2", `Austral Winter` = "3", `Austral Spring`="4")) %>%
  filter(!is.na(Season))  

#split into zones
all_subj_list <- split(all_subj, all_subj$`#utm_zone`)

# Make subject tiles sf
# ulx, uly, lrx, lry,
make_sfc_from_one_zoo <- function(a_tile){
  with(a_tile,
       st_bbox(c(xmin = `#tile_UL_x`, xmax = `#tile_LR_x`, 
                 ymax = `#tile_UL_y`, ymin = `#tile_LR_y`), 
               crs = str_c("+proj=utm +zone=", `#utm_zone`[1], " +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))) %>%
    st_as_sfc()
       
}

make_sfc_from_zoo <- function(a_tibble){
  subject_sfc <- map(transpose(a_tibble), make_sfc_from_one_zoo) %>%
    do.call(c, .)
  
  #put it into a tibble
  a_tibble %>%
    mutate(geometry = subject_sfc) %>%
    st_as_sf()
    
}

all_subj_list_sf_list <- map(all_subj_list, make_sfc_from_zoo)

saveRDS(all_subj_list_sf_list, str_c(write_dir, "all_subj_list_sf_list.Rds"))

all_subj_list_sf_latlong <- map(all_subj_list_sf_list, 
                                        ~st_transform(.x, crs = st_crs(f))) %>%
  reduce(rbind)

saveRDS(all_subj_list_sf_latlong, str_c(write_dir, "all_subj_list_sf_latlong.Rds"))

#group by season/year
coverage_merged_seasonal_annual <- all_subj_list_sf_latlong %>%
  group_by(Year, Season, retirement_reason) %>%
  summarize()

saveRDS(coverage_merged_seasonal_annual, 
        str_c(write_dir, "../level_1/coverage_merged_seasonal_annual.Rds"))

#group by season
coverage_merged_seasonal <- coverage_merged_seasonal_annual %>%
  group_by(Season, retirement_reason) %>%
  summarize()

saveRDS(coverage_merged_seasonal, 
        str_c(write_dir, "../level_1/coverage_merged_seasonal.Rds"))


#group by season/year
coverage_merged_annual <- coverage_merged_seasonal_annual %>%
  group_by(Year, retirement_reason) %>%
  summarize()

saveRDS(coverage_merged_annual, 
        str_c(write_dir, "../level_1/coverage_merged_annual.Rds"))


# #check
# #check
# library(leaflet)
# leaflet(all_subj_list_sf_latlong) %>% 
#   addProviderTiles(providers$Stamen.Terrain) %>%
#   addPolygons()
# 
# library(ggplot2)
# ggplot() +
#   geom_sf(data = all_subj_list_sf_latlong, fill = "lightgrey") +
#   geom_sf(data = f, fill = "darkgreen") +
#   ggthemes::theme_map()
#   

#check by season
by_season <- ggplot() +
  geom_sf(data = coverage_merged_seasonal, mapping = aes(fill = retirement_reason)) +
  facet_wrap(~Season) +
  geom_sf(data = f, fill = "darkgreen", alpha = 0.8) +
  theme_void(base_line_size=0) 
  

jpeg("../../../figures/relaunch_viz/coverage_by_season.jpg")
by_season
dev.off()
#check by year

by_year <- ggplot() +
  geom_sf(data = coverage_merged_annual, mapping = aes(fill = retirement_reason)) +
  facet_wrap(~Year) +
  geom_sf(data = f, fill = "darkgreen", alpha = 0.8) +
  theme_void(base_line_size = 0) +
  theme(panel.grid.major =  element_blank())

jpeg("../../../figures/relaunch_viz/coverage_by_year.jpg")
by_year
dev.off()


by_year_season <- ggplot() +
  geom_sf(data = coverage_merged_seasonal_annual, 
          mapping = aes(fill = retirement_reason)) +
  facet_grid(Season~Year) +
  geom_sf(data = f, fill = "darkgreen", alpha = 0.8) +
  theme_void(base_line_size = 0) +
  theme(panel.grid.major =  element_blank())

jpeg("../../../figures/relaunch_viz/coverage_by_year_season.jpg")
by_year_season
dev.off()


