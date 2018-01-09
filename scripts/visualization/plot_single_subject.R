#Load libraries and data
library(ggmap)
library(jpeg)
library(grid)
library(gridExtra)
library(egg)

source("./load_prep_classifications.R")

match_corners <- function(tile_lower_left_x_UTM,
                          tile_lower_left_y_UTM,
                          tile_upper_right_x_UTM,
                          tile_upper_right_y_UTM,
                          zone = 10,
                          to_crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"){
  arast <- raster(nrows=2, ncols=2,
                  xmn = tile_lower_left_x_UTM, ymn = tile_lower_left_y_UTM,
                  xmx = tile_upper_right_x_UTM, ymx =  tile_upper_right_y_UTM,
                  crs = crs(paste0("+proj=utm +zone=",zone," +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  
  suppressWarnings(arast_temp <- projectRaster(arast, crs=to_crs))
  extent(arast_temp)
}

### Plot one subject ####
plot_one_subject <- function(single_subject, rescale=FALSE,
                             lat_adj = 0.01, lon_adj = 0.01,
                             zoom = 13, poly_limits=FALSE){

  akp <- classifications_df %>% filter(zooniverse_id==single_subject)
  corner_ext <- match_corners(akp$tile_lower_left_x_UTM[1],
                              akp$tile_lower_left_y_UTM[1],
                              akp$tile_upper_right_x_UTM[1],
                              akp$tile_upper_right_y_UTM[1],
  )
  
  #akp_map <- get_map(c(min(akp$long), min(akp$lat), max(akp$long), max(akp$lat)),
  #                 zoom=zoom)
  akp_map <- get_map(c(corner_ext[1], corner_ext[3], corner_ext[2], corner_ext[4]),
                   zoom=zoom)
  
  p <- ggmap(akp_map) +
    geom_polygon(data = akp, 
                 mapping=aes(x=long, y=lat, group=group,fill=factor(threshold))) +
    scale_fill_discrete(guide=guide_legend(title="Min. # Users")) +
    xlab("") + ylab("")
    
  if(rescale) p <- p+
      xlim(corner_ext[1]-lon_adj, corner_ext[2]+lon_adj) +
      ylim(corner_ext[3]-lat_adj,corner_ext[4]+lat_adj) 
    
  if(poly_limits) p <- p+
    xlim(min(akp$long)-lon_adj, max(akp$long)+lon_adj) +
    ylim(min(akp$lat)-lat_adj,max(akp$lat)+lat_adj) 
  
  p
}

plot_img <- function(single_subject){
  akp <- classifications_df %>% filter(zooniverse_id==single_subject)
  url <- akp$image_url[1]
  url <- gsub("www", "static.zooniverse.org/www", url)
  url <- gsub("http", "https", url)
  z <- tempfile()
  download.file(url, z, mode = 'wb')
  img <- readJPEG(z)
  rasterGrob(img) 
#  file.remove(z)
}

plot_one_subject_and_image <- function(single_subject, ...,
                                       widths=c(1,1.225), nrow=1, ncol=2){
  
  a1 <- plot_one_subject(single_subject,  ...) + theme_void()
  a2 <- plot_img(single_subject)
  grid.arrange(a2, a1 , nrow=1, ncol=2, widths=c(1, 1.225)) 
}

#https://static.zooniverse.org/www.floatingforests.org/subjects/53e2f5a94954734d8b441900.jpg
plot_one_subject("AKP00004zp")

#with it's image
#unique((classifications_df %>% filter(threshold>10))$zooniverse_id)

plot_one_subject_and_image("AKP00025mh",  zoom=12, poly_limits=TRUE,
                           lon_adj = 0, lat_adj = 0)

# #
# writeOGR(classifications %>% filter(threshold==6), dsn = "../../data/output/consensus_shapefiles/ca_zone_10_threshold_6.kml", layer="kelp", driver="KML")
