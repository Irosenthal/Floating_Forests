library(rayshader)
library(dplyr)
library(sf)
library(ggplot2)
library(patchwork)
library(raster)
library(RStoolbox)
source("../data_pipeline/_pipeline_functions.R")


#the relevant directories
read_dir <- "../../../data/relaunch_data/level_0/sf_tiles"
write_fig_dir <-  "../../../figures/relaunch_viz/tile_vids/"

ff_relaunch_subjects <- readRDS("../../../data/relaunch_data/level_0/ff_relaunch_subjects.Rds")

f <- st_read("../../../data/coastlines/falklands/Falklands.shp")
files <- list.files(read_dir, full.names=TRUE)


rayshade_one_tile <- function(a_subj, width = 6,
                              height = 6, scale = 300, 
                              viridis_option = "B",
                              windowsize = c(1000,800),
                              useSubj= FALSE, plotSubj = useSubj,
                              ...){

   plot_gg(ggplot_one_tile(a_subj, viridis_option = viridis_option), 
      multicore=TRUE,
      width=width,
      height=height,
      windowsize = windowsize,
      scale=scale, ...)
  
}

ggplot_one_tile <- function(a_subj, viridis_option = "B",
                            useSubj= FALSE, plotSubj = useSubj){
  
  #get the filename
  filename <- paste0(read_dir, "/", a_subj, ".Rds")
  
  #load the file
  a_file <- readRDS(filename) %>% filter(threshold>3) 
  
  #fix the landsat S bug
  patch <- a_file %>% 
    mutate(geometry = geometry+c(0,1e7)) %>%
  st_set_crs(st_crs(a_file))

  
  #get img in case we need it
  if(useSubj || plotSubj){
    img <- rasterizeFFImage(a_subj)
    extent(img) <- extent(img) + c(0,0,1e7, 1e7)
  }
  
 
  #get transformed patches and clip coastline
  t_patch <- st_transform(patch, st_crs(f))
  f_patch <- st_crop(f, t_patch)
  
  #crop things if we are using full subject size
  if(useSubj){
    
    patch <- st_crop(patch, img)
    
    cropBox <- st_as_sfc(st_bbox(img))  %>% 
      st_transform(st_crs(f))
    
    f_patch <- st_crop(f, cropBox)
  }
  
  if(plotSubj){
    gg_patch <- ggRGB(img) 
    
    ## clip classifications
    # clip and backtransform
    patch <- st_difference(t_patch, f_patch) %>%
      st_transform(st_crs(a_file_fixed)) %>%
      dplyr::select(threshold, geometry)

  }else{
    gg_patch <- ggplot() +    
      geom_sf(data = f_patch, fill = "lightgrey") 
    
    patch <- t_patch
  }
  
  gg_patch <- gg_patch +
    geom_sf(data = patch, aes(color = threshold, fill = threshold)) +
    theme(panel.background = element_blank()) +
    scale_color_viridis_c(option = viridis_option) +
    scale_fill_viridis_c(option = viridis_option)# +
   # ggtitle(paste0("Gmap Link: ", make_short_link_from_tile(patch)))
  
  #correcting lims
  if(useSubj && !plotSubj){
    
    b <- st_bbox(cropBox)
    
    gg_patch <- gg_patch +
      xlim(b[1], b[3]) +
      ylim(b[2], b[4])
  }
  
  #add landscape at the end to crop visually
  if(!plotSubj){
    gg_patch <- gg_patch +    
      geom_sf(data = f_patch, fill = "lightgrey") 
  }
  
  gg_patch

}

make_short_link_from_tile <- function(a_tile){
  
  pt <- as.character(st_centroid(a_tile$geometry[[1]]))
  
  vgd_LinksShorten(longUrl = paste0(
    "https://www.google.com/maps/place/", pt[2], ",", pt[1]),
                   showRequestURL = TRUE)
}

###########
#demo
###########

#ggplot
ggplot_one_tile(15113822)
ggplot_one_tile(15113822, useSubj = TRUE)
ggplot_one_tile(15113822, useSubj = TRUE, plotSubj = FALSE)

#together with raster!
ggRGB(rasterizeFFImage(15113822))+theme_void() + ggplot_one_tile(15113822, useSubj = TRUE, plotSubj = FALSE)


#rayshade a snapshot
rayshade_one_tile(15113822)
render_snapshot(paste0(write_fig_dir, "/", "15113822.png"), clear = TRUE)
render_movie(paste0(write_fig_dir, "/", "15113822_flat.mp4"))


rayshade_one_tile(15113822, useSubj=TRUE)
render_snapshot(paste0(write_fig_dir, "/", "15113822_withsubj.png"), clear = TRUE)


#rayshade a movie
rayshade_one_tile(15113822,  windowsize = c(1000,800), useSubj=TRUE)
render_camera(zoom = 0.5, theta = 130, phi = 35)
render_movie(paste0(write_fig_dir, "/", "15113822b.mp4"))


#rayshade a movie
rayshade_one_tile(15113822,  windowsize = c(1000,800))
render_camera(zoom = 0.5, theta = 130, phi = 35)
render_movie(paste0(write_fig_dir, "/", "15113822a.mp4"))



