#'#############
#'
#'  File to make animations of 
#'  how kelp looks over the CA coastline
#'  at different levels of consensus classification
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

#######
##get a map of CA coastline to plot against ####
#####
#Just the coastline
ca_coastline <- readOGR("../../data/coastlines/California", layer="California")
ca_coastline <- crop(ca_coastline, extent(classifications))

#a raster of the coast
ext <- projectExtent(classifications, CRS("+proj=longlat +datum=WGS84"))
ca_map <- get_map(as.vector(bbox(ext)), zoom=7)

#######
#With vector coast ####
#####
p <- ggplot() + 
  geom_polygon(data=classifications_df %>% filter(threshold<16),  
               mapping=aes(x=long, y=lat, group=group,
                           frame = threshold, fill=factor(threshold))) +
  theme_bw(base_size=14) +
  xlab("") + ylab("") +
  ggtitle("Minimum # Users Threshold = ") +
  geom_path(ca_coastline, mapping=aes(x=long, y=lat, group=id), color="lightgrey", alpha=0.7) +
  xlim(as.vector(bbox(ext))[1], as.vector(bbox(ext))[3]) +
  ylim(as.vector(bbox(ext))[2], as.vector(bbox(ext))[4]) +
  coord_map() +
  scale_fill_discrete(guide="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



#Make a gganimation of the different thresholds

gg_animate(p, "../../figures/consensus_gifs/threshold_polys_coast.gif",
           title_frame=TRUE, width=800, height=600)

#Monterey
gg_animate(p + xlim(-122.652633, -121.563554) + ylim(36.318073, 37.462106), 
           "../../figures/consensus_gifs/threshold_polys_monterey.gif",
           title_frame=TRUE, width=800, height=600)

#big sur
gg_animate(p + xlim(-122.014459,  -120.497787) + ylim(35.131980, 36.406971), 
           "../../figures/consensus_gifs/threshold_polys_bigsur.gif",
           title_frame=TRUE, width=800, height=600)

#north coast

gg_animate(p + xlim(-124.001673, -122.772273) + ylim(38.115699,39.176660), 
           "../../figures/consensus_gifs/threshold_polys_northcoast.gif",
           title_frame=TRUE, width=800, height=600)

#####
## Rasters of the Coast ####

##attempt
pmap <- ggmap(ca_map) +
  geom_polygon(data=classifications_df %>% filter(threshold<16),  
               mapping=aes(x=long, y=lat, group=group,
                           frame = threshold), fill="red") +#, fill=factor(threshold))) +
  theme_bw(base_size=14) +
  xlab("") + ylab("") +
  ggtitle("Minimum # Users Threshold = ") +
  scale_fill_discrete(guide="none") 



gg_animate(pmap, "../../figures/consensus_gifs/threshold_polys_rast.gif",
           title_frame=TRUE, width=800, height=600)


#Monterey
gg_animate(pmap + xlim(-122.652633, -121.563554) + ylim(36.318073, 37.462106), 
           "../../figures/consensus_gifs/threshold_polys_monterey_rast.gif",
           title_frame=TRUE, width=800, height=600)


#big sur
gg_animate(pmap + xlim(-122.014459,  -120.497787) + ylim(35.331980, 36.406971), 
           "../../figures/consensus_gifs/threshold_polys_bigsur_rast.gif",
           title_frame=TRUE, width=800, height=600)

#north coast
gg_animate(pmap + xlim(-124.001673, -122.772273) + ylim(38.015699,39.176660), 
           "../../figures/consensus_gifs/threshold_polys_northcoast_rast.gif",
           title_frame=TRUE, width=800, height=600)


#some faceting
png("../gifs/threshold_polys_northcoast_facet.gif", width=1024, height=768)
p + 
  facet_wrap( ~ threshold)
dev.off()