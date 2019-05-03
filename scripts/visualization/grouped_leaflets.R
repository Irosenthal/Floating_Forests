library(sf)
library(tidyverse)
library(rmapshaper)
library(leaflet)
library(htmlwidgets)

#test with leaflet
#all_kelp_sf <- readRDS("../../data/relaunch_data/level_1/all_kelp_sf.Rds")
#merged_sf_latlong <- readRDS("../../data/relaunch_data/level_1/merged_sf_latlong.Rds")

#seasonal <- readRDS("../../data/relaunch_data/level_1/all_annual_kelp_by_season.Rds")
#seasonal <- seasonal2 %>% dplyr::filter(threshold==8)
season_simp <- readRDS("../../data/relaunch_data/level_1/all_annual_kelp_by_season_simplified.Rds")
decade <- readRDS("../../data/relaunch_data/level_1/all_annual_kelp_decadal.RDS") %>%
  dplyr::filter(threshold==6)


#simplify polygons for leaflet

season_simp <- ms_simplify(seasonal2, keep = 0.5)
decade_simp <- ms_simplify(decade, keep = 0.75)


#saveRDS(season_simp,"../../data/relaunch_data/level_1/all_annual_kelp_by_season_simplified.Rds" )



#for loop version from SO - works but may be slowing things down
#use if there are many groups and runtime is not an issue
groups <- as.character(unique(season_simp$Season))

map <- leaflet(season_simp) %>% addTiles(group = "OpenStreetMap")
for(g in groups){
  d = season_simp[season_simp$Season == g, ]
  map = map %>% addPolygons(data = d,
                            color = "green",
                                 group = g)
  
}

#add layer control separately so it doesn't crash
map <- map %>% addLayersControl(overlayGroups = groups)
#export as html
saveWidget(map, file="../../data/relaunch_data/falklands_course.html", selfcontained = F)


###No loop this time but need to set groups manually
#one polygon per factor level
#use when control is more important than automation - allows custom group names

map2 <- leaflet() %>%
  addPolygons(data = season_simp %>% filter(Season == "Austral Summer"), group = "Austral Summer", color = "green") %>%
  addPolygons(data = season_simp %>% filter(Season == "Austral Spring"), group = "Austral Spring", color = "green") %>%
  addPolygons(data = season_simp %>% filter(Season == "Austral Fall"), group = "Austral Fall", color = "green") %>%
  addPolygons(data = season_simp %>% filter(Season == "Austral Winter"), group = "Austral Winter", color = "green") %>%
  addLayersControl(overlayGroups = c("Austral Summer", "Austral Spring", "Austral Autumn", "Austral Winter"),
                   options = layersControlOptions(collapsed = FALSE))

map2




