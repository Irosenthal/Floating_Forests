#Make tile library of the Falklands

library(tiler)
library(raster)
library(stringr)

read_dir <- "../../../data/relaunch_data/level_0/"
write_fig_dir <-  "../../../figures/relaunch_viz/falklands_tiles/"

pal <- colorRampPalette(c("lightgreen", "darkgreen"))(13)
plot(1:13, rep(1,13), col = pal, pch = 19)

#reproject for leaflet
f <- raster(str_c(read_dir, "merged_falkands.tif"))
f2 <- projectRaster(f, crs = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
writeRaster(str_c(read_dir, "merged_falkands_leaflet.tif"), overwrite=TRUE)

tile(str_c(read_dir, "merged_falkands_leaflet.tif"), write_fig_dir, "0-7",
     col = pal)

# ### view
# tile_viewer(write_fig_dir, zoom = "0-7")
# 
# library(leaflet)
# #in the relevant directory
# #python -m SimpleHTTPServer 8000 
# tilesUrl <- "http://localhost:8000/{z}/{x}/{y}.png"
# 
# col_default <- colorRampPalette(c("#7F3B08", 
#                                   "#B35806", "#E08214", "#FDB863", "#FEE0B6", "#F7F7F7", 
#                                   "#D8DAEB", "#B2ABD2", "#8073AC", "#542788", "#2D004B"))(10)
# 
# 
# #tiles <- "https://leonawicz.github.io/tiles/us48lr/tiles/{z}/{x}/{y}.png"
# #tiles <- "../../../figures/relaunch_viz/falklands_tiles/{z}/{x}/{y}.png"
# leaflet() %>% 
#   #addTiles() %>%
#   addProviderTiles("Stamen.Toner") %>% 
# #  addProviderTiles("Esri.WorldImagery") %>%
#   addTiles(urlTemplate = tilesUrl, options = tileOptions(opacity = 0.8))  %>%
#   addLegend(colors = col_default,   position = "bottomright",
#             labels = as.character(4:13), title = "Users Agreeing") %>%
#   setView(-59, -52, 0)
