source("./FFmethods_fix.R")
library(R.matlab)
library(rgdal)
library(readr)
library(dplyr)
library(beepr)

#First, create a dataframe to nest spatial poly data frames in
classifications <- readRDS("../data/products/classifications_master")
beep(1)
subjects <- readRDS("../data/products/subjects_master") 
beep(1)
scene_meta <- read_csv("../data/products/scene_metadata.csv") 
beep(1)

saveRDS(subjects, "../data/products/subjects_master")


subjects <- subjects %>%
   dplyr:: select(zooniverse_id, UTM_ZONE)

#so it plays nice with some functions  FFmethods_fix, streamline this in the future
#remnant from old pipeline
scene_with_FF_UTM <- subjects


classifications <- left_join(classifications, subjects, by = c("subject_zooniverse_id" = "zooniverse_id"))

# for playing with 1 scene at a time
classifications <- filter(classifications, as.character(utm) == "LE70440351999204EDC01")
classifications <- filter(classifications, as.character(UTM_ZONE) == "10")



#remove any rows that have NAs
classifications_clean <- rm_na(classifications, "relPath")
classifications_clean <- rm_na(classifications_clean, "startingPoint_x")
classifications_clean <- rm_na(classifications_clean, "startingPoint_y")
classifications_clean <- mutate(classifications_clean, upper_right_x = tile_upper_right_x_UTM)
classifications_clean <- mutate(classifications_clean, upper_right_y = tile_upper_right_y_UTM)
classifications_clean <- mutate(classifications_clean, lower_left_x = tile_lower_left_x_UTM)
classifications_clean <- mutate(classifications_clean, lower_left_y = tile_lower_left_y_UTM)
classifications_clean <- rm_na(classifications_clean, "upper_right_x")
classifications_clean <- rm_na(classifications_clean, "upper_right_y")
classifications_clean <- rm_na(classifications_clean, "lower_left_x")
classifications_clean <- rm_na(classifications_clean, "lower_left_y")


#make a dataframe where each image has its classification data nested within it

nested_classifications <- split(classifications_clean , f = classifications_clean$subject_zooniverse_id)

#now lapply to convert to SPDFS
#This is where you need a subject table called scene_with_FF_UTM
sp_classifications_list <- lapply(nested_classifications, getSpatialPolysDataFrameForOneImage)
#saveRDS(sp_classifications_list, "../data/intermediates/nested_classifications_spatial")

sp_classifications_df <- data.frame(SPDF = matrix(ncol = 0, nrow = length(sp_classifications_list)))

sp_classifications_df$SPDF <- sp_classifications_list

saveRDS(sp_classifications_df, "../data/products/sp_classifications_df.rds")
classdf <- readRDS("../data/products/sp_classifications_df.rds")

#load calibration data and select only rows with kelp
caKelp <- readMat("../data/kelp_043035_7_24_1999.mat")
caKelp <- caKelp$kelp[which(caKelp$kelp[,3]>0),]

#the proj4string is the same for all of them, so I only need to do this once - is there a better way?
sr <- "+proj=longlat +datum=WGS84"
#"+proj=utm +zone=10 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

caKelp.spoints <- SpatialPoints(caKelp[,2:1], 
                                proj4string=CRS(sr))

#make sure to adjust utm zone if required
caKelp.spoints <- spTransform(caKelp.spoints, "+proj=utm +zone=11 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

################
sp_classifications_df <- readRDS("../data/products/sp_classifications_df_one_scene.rds")
#plot one at a time for now
#replace the row number in sp_classifications_df[[i,1]] for each image
x <- 5

class_sp <- sp_classifications_df[[x,1]]


caKelp.spoints_crop <- crop(caKelp.spoints, extent(sp_classifications_df[[x,1]]))


#caKelp.spoints <- spTransform(caKelp.spoints, CRS("+init=epsg:3857"))

#get the bounds for plotting from the polygons data frame

longBounds <- bbox(sp_classifications_df[[x,2]])[1,]
latBounds <- bbox(sp_classifications_df[[x,2]])[2,]

#plot them together
plot(caKelp.spoints, xlim=longBounds, ylim=latBounds, pch=20, cex=0.5)
plot(sp_classifications_df[[x,2]], add=T)


######################
#more visualization:

lims <- as.numeric(sp_classifications_df[[x,1]]@data[1,c(10,8,11,9)])
coasts <- readOGR("../../data/GSHHS_shp/h", layer="GSHHS_h_L1",  useC=FALSE)


plot(coasts, xlim=lims[1:2], ylim=lims[3:4], lwd=4)
plot(sp_classifications_df[[x,1]], add=T)

testdf <- as.data.frame(sp_classifications_df[[x,1]][1,])

tileBrick <- rasterizeFFImage(sp_classifications_df[[x,1]][1,])

#reproject to longlat in order to add shapefile of coastline
sr <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
tileBrick <- projectRaster(tileBrick, crs = sr)


plotRGB(tileBrick)

plot(coasts, xlim=lims[1:2], ylim=lims[3:4], lwd=4, add=T)
plot(caKelp.spoints, xlim=longBounds, ylim=latBounds, pch=20, cex=0.5, 
     add=T, col=rgb(1,0,0,alpha=0.1))

plot(sp_classifications_df[[x,1]], add=T)
plot(spTransform(sp_classifications_df[[x,1]], CRS("+proj=longlat +datum=WGS84")), add=T)

proj_test <- spTransform(sp_classifications_df[[x,1]], CRS(sr))



###################################
#rasterize

r <- raster(crs=sp_classifications_df[[x,1]]@proj4string, ext=extent(sp_classifications_df[[x,1]]), ncols = 400, nrows=364)
rastLayer <- rasterize(SpatialPolygons(sp_classifications_df[[x,1]]@polygons), r, fun="count")


plotRGB(tileBrick)
plot(rastLayer,
     legend.args=list(text='# of Users\nSelecting',
                      side=3, font=7.2, line=0.5, cex=0.8), add=T)


longBounds <- bbox(rastLayer)[1,]
latBounds <- bbox(rastLayer)[2,]

#Jarrett's viz parameters
zoomLongBounds <- bbox(SpatialPolygons(sp_classifications_df[[x,1]]@polygons))[1,]
zoomLatBounds <- bbox(SpatialPolygons(sp_classifications_df[[x,1]]@polygons))[2,]

hasData <- which(!is.na(as.matrix(rastLayer)), arr.ind=TRUE)
rastZoomXBounds <- c(min(hasData[,2]), max(hasData[,2]))
rastZoomYBounds <- c(min(hasData[,1]), max(hasData[,1]))
zoomExtent <- extent(c(zoomLongBounds, zoomLatBounds))

#plot them together

plot(crop(rastLayer, extent(c(zoomLongBounds, zoomLatBounds))),
     legend.args=list(text='# of Users\nSelecting', 
                      side=3, font=2.2, line=0.5, cex=0.8)
)

plot(caKelp.spoints, xlim=zoomLongBounds, ylim=zoomLatBounds, pch=20, cex=0.5, add=T)

###########
caKelp.raster <- rasterize(caKelp.spoints, r, fun="count")


#See what this looks like
combined.raster <- overlay(caKelp.raster, rastLayer, fun="sum")

plot(crop(combined.raster, zoomExtent))
hist(as.matrix(combined.raster), breaks=1:16)
############
# Functions to caculate areas from Jarrett
#############
getKelpPixelsFromRaster <- function(rast, threshold=1){
  length(which(as.matrix(rast)>=threshold))
}

getKelpPixelsFromRasterV <- Vectorize(getKelpPixelsFromRaster, "threshold")

getUsers <- function(a,b) {
  if(is.na(a)) return(NA)
  b
}


kelpPixels <- data.frame(users=1:15, 
                         kelpPixels = getKelpPixelsFromRasterV(rastLayer, 1:15),
                         goodKelpPixels = getKelpPixelsFromRasterV(combined.raster, 1:15))
kelpPixels$kelpArea <- kelpPixels$kelpPixels*0.03*0.03

kelpPixels <- data.frame(users=1:15, 
                         kelpPixels = getKelpPixelsFromRasterV(rastLayer, 1:15))
kelpPixels$kelpArea <- kelpPixels$kelpPixels*0.03*0.03


plot(goodKelpPixels ~ kelpPixels, data=kelpPixels, type="n")
text(kelpPixels$kelpPixels, kelpPixels$goodKelpPixels, labels=1:15)
plot(goodKelpPixels ~ users, data=kelpPixels)


plot(kelpPixels ~ users, data=kelpPixels)
abline(h=getKelpPixelsFromRaster(caKelp.raster))








