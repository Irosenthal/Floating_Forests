library(dplyr)
library(rgeos)
library(sp)
library(raster)
library(rgdal)

#Data sources
coasts <- readOGR("../../data/gshhg-shp-2.3.4/GSHHS_shp/h", layer="GSHHS_h_L1",  useC=FALSE)


#Make a cropped CA coastline
#extent( ll_lon, ur_lon, ll_lat, ur_lat )
california <- crop(coasts, 
                   extent(-124.4, -114.1, 32.5, 42))

plot(california,  bg="lightblue", col="brown")
writeOGR(california, "../data/coastlines/California", layer="California", driver="ESRI Shapefile")

#Make a cropped Tasmania coastline
tasmania <- crop(coasts,
                 extent(143.67, 148.8, -43.8785, -39.1624))

plot(tasmania,  bg="lightblue", col="brown")
writeOGR(tasmania, "../data/coastlines/tasmania", layer="Tasmania", driver="ESRI Shapefile")

#Make a cropped Falklands coastline


falklands <- crop(coasts,
                 extent(-56.5, -62.5, -50.9, -53))

plot(falklands,  bg="lightblue", col="brown")
writeOGR(falklands, "../data/coastlines/falklands", layer="Falklands", driver="ESRI Shapefile")
