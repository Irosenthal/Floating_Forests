#'#############
#'
#'  File to make animations of 
#'  how kelp looks over time
#'  
#'#############


##Load libraries and data
library(ggplot2)
library(ggmap)
library(gganimate)
source("./load_prep_classifications.R")

classifications_list <- get_classifications(return_value="both")
classifications <- classifications_list$spdf
classifications_df <- classifications_list$df
rm(classifications_list) #because it's big!
gc()

###
#plot time in Monterey ####
#ggmap(ca_map) + xlim(-122.0, -121.85) + ylim(36.5, 36.65)
monterey_map <- get_map(c(-122.0, 36.5, -121.85, 36.65), maptype="terrain-background", 
                        source="stamen")

mont <- crop(classifications, extent(-122.0, -121.85, 36.5, 36.65)) 
mont@data$id <- sapply(mont@polygons, function(x) x@ID)


mont_df <- mont %>% tidy %>%
  left_join(mont@data) %>%
  mutate(year = year(scene_timestamp),
         season =gsub("[1,2][0,9,9][0-9][0-9].", "", quarter)) %>%
  mutate(season = forcats::fct_recode(season, Winter = "1", 
                                      Spring = "2",
                                      Summer = "3",
                                      Fall = "4"))

mont_map <- ggmap(monterey_map) +
  geom_polygon(data=mont_df %>% filter(threshold==6),  
               mapping=aes(x=long, y=lat, group=group, frame=quarter), 
               color="black", fill="red") +
  xlab("") + ylab("")

gg_animate(mont_map, 
           "../../figures/timeseries_gifs/monterey_over_time_rast.gif",
           title_frame=TRUE, width=800, height=600)

png("../../figures/timeseries_gifs/monterey_over_time_facet.gif", width=800, height=600)
mont_map + 
  facet_grid(season ~ year)
dev.off()
