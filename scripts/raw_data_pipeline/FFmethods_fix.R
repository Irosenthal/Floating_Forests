library(RJSONIO)
library(RCurl)
library(dplyr)
library(readr)
library(maptools)
library(raster)
library(jpeg)
library(rgeos)

getImageInfo <- function(imageID, subjURL = "https://api.zooniverse.org/projects/kelp/subjects/"){
  fromJSON(getURL(paste0(subjURL, imageID)))
}

#can probably do everything in LL now that it's all attached to the classifications

#url <- "http://www.floatingforests.org/subjects/53e61b1149547333204f5c00.jpg"
#download.file(url, basename(url), mode = 'wb')

#FIX FOR SKEWED IMAGES: import in utm and then feed it the correct coordinates.
#NOTE: scene_with_FF_UTM is likely to change as project evolves/becomes automated, just make sure to
#feed it the correct corner coordinates via meta_info
rasterizeFFImage <- function(arow, proj= "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", 
                             correct_meta = scene_with_FF_UTM){
  imageInfo <- getImageInfo(arow$subject_zooniverse_id)
  meta_info <- dplyr::filter(correct_meta, zooniverse_id == arow$subject_zooniverse_id)
  #meta_info <- correct_meta
  loc <- gsub("http://", "https://static.zooniverse.org/", imageInfo$location)
  
  download.file(loc, basename(loc), mode = 'wb')
  
  img <-brick(basename(loc),              
              crs=proj)
  #old column refs - these will change. make sure the final version is consistant.
  #img@extent = extent(c(meta_info$lower_left_x_utm, meta_info$upper_right_x_utm, 
                        #meta_info$lower_left_y_utm, meta_info$upper_right_y_utm))
  
  #img@extent = extent(c(meta_info$CORNER_LL_LON_PRODUCT, meta_info$CORNER_UR_LON_PRODUCT, 
                        #meta_info$CORNER_LL_LAT_PRODUCT, meta_info$CORNER_UR_LAT_PRODUCT))
  
  img@extent = extent(c(meta_info$tile_upper_left_x_LL, meta_info$tile_lower_right_x_LL, 
                        meta_info$tile_lower_left_y_LL, meta_info$tile_upper_right_y_LL))
  
  return(img)
  
}

#old "+proj=utm +datum=WGS84"


#rasterizeFFImage <- function(arow, proj="+proj=utm +datum=WGS84"){
#  imageInfo <- getImageInfo(arow$subject_zooniverse_id)
 # loc <- gsub("http://", "https://static.zooniverse.org/", imageInfo$location)
  
  #img <-brick(loc,              
   #            crs=proj)
  #
  #img@extent = extent(c(arow$lower_left_x, arow$upper_right_x, 
                       # arow$lower_left_y, arow$upper_right_y))

  #return(img)
  
#}

#images are 532x484
#takes a single relative path and a row of data and returns a polygon
getPoly <- function(arow, polyPath, xpixels = 532, ypixels=484){

  #transform the row into a 
  polyPath <- polyPath[-length(polyPath)]
  polyPath <- t(sapply(polyPath, function(x){
    x <- gsub("^(.*)\\[", "", x)
    x <- gsub("\\'", "", x)
    x <- strsplit(x, ",")[[1]]
    as.numeric(x)}))
  
  rownames(polyPath) <- 0:(nrow(polyPath)-1)
  
  #make it absolute points  
  polyPath[,2] <- -1*(polyPath[,2]) #invert b/c y is opposite in the FF DB
  
  polyPath[,1] <- cumsum(polyPath[,1])
  polyPath[,2] <- cumsum(polyPath[,2])
  
  #close the polygon
  polyPath <- rbind(polyPath, polyPath[1,])
  
  #give it an absolute position
  polyPath[,1] <- polyPath[,1]+arow$startingPoint_x
  polyPath[,2] <- polyPath[,2]+ypixels-arow$startingPoint_y #invert b/c y is opposite in the FF DB
  
  #give it a position on the map
  xlen <- arow$upper_right_x - arow$lower_left_x
  ylen <- arow$upper_right_y - arow$lower_left_y
  polyPath[,1] <- polyPath[,1]*xlen/xpixels + arow$lower_left_x
  polyPath[,2] <- polyPath[,2]*ylen/ypixels + arow$lower_left_y
  #plot(polyPath[,1], polyPath[,2], type="l")
  
  #turn it into a sp::Polygon
  ret <- Polygon(polyPath, hole=F)
  ret
}

#takes a dataframe and returns a Polygons object
#with user_name as default for ID
getPolys <- function(aframe, idForPolys=aframe$classification_id[1]){
  polyPaths <- strsplit(as.character(aframe$relPath), "\\]")
  polys <- vector("list", length(polyPaths))
  
  for(i in 1:length(polyPaths)){
    polys[[i]] <- getPoly(aframe[i,], polyPaths[[i]])
  }
  
  p <- Polygons(polys, ID=idForPolys)
  
}

#takes a dataframe and returns a SpatialPolygons object
getSpatialPolysForOneImage <- function(aframe, proj= "+proj=utm +zone=10 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs") {
  #old: "+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  polys <- plyr::dlply(aframe, "classification_id", getPolys)
  SpatialPolygons(polys, proj4string=CRS(proj))
}

#takes a dataframe and returns a SpatialPolygons object
getSpatialPolysDataFrameForOneImage <- function(aframe, proj="+proj=utm +zone=10 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs"){
  spatialPolys <- getSpatialPolysForOneImage(aframe, proj)
 #old "+proj=merc +datum=WGS84"
  newFrame <- aframe %>%
    group_by(classification_id, user_name,subject_zooniverse_id,created_at,
             rejection_water_percent,rejection_cloud_percent,clouds,
             upper_right_x, upper_right_y, lower_left_x, lower_left_y, DATE_ACQUIRED.x) %>%
    summarise(nBeds = length(user_name)) %>%
    #spit out zooniverse id for debugging
    print(subject_zooniverse_id) %>%
  
  ungroup()
  
  row.names(newFrame) <- newFrame$classification_id

  ret <- SpatialPolygonsDataFrame(spatialPolys, data=as.data.frame(newFrame))
  ret@bbox <- matrix(c(aframe$lower_left_x[1], aframe$lower_left_y[1],
                       aframe$upper_right_x[1], aframe$upper_right_y[1]), nrow=2)
  
  ret
}

#takes a list of indices of users and returns the 
#total or shared # of pixels in a raster
getAreaFromUserCombn <- function(idx, spdf=polysData, type="shared"){
  
  #aggregate all polygons to one raster
  
  combnR <- raster(crs=spdf@proj4string, ext=extent(spdf))
  rastLayerCombn <- rasterize(SpatialPolygons(spdf@polygons)[idx], combnR, 
                              fun="count")  
  if(type=="total") {
    return(length(which(as.matrix(rastLayerCombn)>=1)))
  }
  
  #defaults to shared
  length(which(as.matrix(rastLayerCombn)==length(idx)))
  
}

######Get correct corners?
getCorrectCorners <- function(subject="AKP00016e6",
                              n_row=20, n_cols=20,
                              scene_y_pixels =7981, 
                              scene_x_pixels = 7271,
                              proj="+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"){

  imageInfo <- getImageInfo(subject)
  scene <-imageInfo$metadata$base_file_name
  lower_left <- imageInfo$metadata$lower_left #offset of row and col num
  upper_right <- imageInfo$metadata$upper_right

  #get row, col info
  row_no <- imageInfo$metadata$row_no
  col_no <- imageInfo$metadata$col_no


  #I know row and col #
  row_no <- imageInfo$metadata$row_no
  col_no <- imageInfo$metadata$col_no

  #I can calculate central lat/long
  cent <- c((lower_left[1] + upper_right[1])/2, 
          (lower_left[2] + upper_right[2])/2)


  #i can calculate central pixels
  cent_y_pixel <- (scene_y_pixels / n_row) * row_no 
  cent_x_pixel <- (scene_x_pixels / n_cols) *col_no


  #I can calculate image size?
  image_chunk_width = scene_x_pixels / n_row
  image_chunk_height = scene_y_pixels / n_cols

  ll_pixels <- c(cent_x_pixel-image_chunk_width/2, 
               cent_y_pixel-image_chunk_height/2)
  ur_pixels <- c(cent_x_pixel+image_chunk_width/2, 
               cent_y_pixel+image_chunk_height/2)

  #i can calculate rx = cdx/cpx
  conv_deg <- abs(c((upper_right[1] - lower_left[1])/image_chunk_width, 
                  (upper_right[2] - lower_left[2])/image_chunk_height))

  #I can calculate the scene xmin and ymin
  #cdx = cpx*d/p + xd0
  scene_ll <- c(cent[1]-cent_x_pixel*conv_deg[1],
              cent[2]-cent_y_pixel*conv_deg[2])

  scene_ur <- c(scene_ll[1]+scene_x_pixels*conv_deg[1], 
              scene_ll[2]+scene_y_pixels*conv_deg[2])

  #OK, now make a raster
  library(raster)
  ascene <- raster(nrow=scene_y_pixels, ncol=scene_x_pixels, 
                 xmn=scene_ll[1], xmx=scene_ur[1], 
                 ymn=scene_ll[2], ymx=scene_ur[2])
  #for CA
  projection(ascene) <- proj

  newx <- xFromCol(ascene, c(ll_pixels[1], ur_pixels[1]))
  newy <- yFromRow(ascene, c(ll_pixels[2], ur_pixels[2]))

  list(ll=c(newx[1], newy[1]), ur = c(newx[2], newy[2]))
}


#For overlap analysis
getOverlapPixels <- function(userPolygonDataFrame, 
                             llx, lly, urx, ury,
                             xPixels = 532, yPixels = 484,
                             output="pixels", defaultCellValue=0){
  
  #make a grid we'll be useing to count overlap with
  GT <- GridTopology(c(llx, lly), 
                     c((urx-llx)/xPixels,
                       (ury-lly)/yPixels),
                     c(xPixels+1, yPixels+1))
  
  SPG <- SpatialGrid(GT, proj4string=projection(userPolygonDataFrame))
  
  #get the overlap pixel numbers with the grid and each polygon
  o <- over(userPolygonDataFrame, SPG, returnList=T, fn=sum)
  oVec <- unlist(o) 
  
  #count number of polygons overlapping each pixel in the grid
  pixelCount <- sapply(unique(oVec), function(x) sum(oVec==x))
  
  if(output=="grid") {
    #first, make a vector of counts matching the grid
    #by getting all counts, and then using their indices to make a count
    #vector that matches the length of the grid.
    ct <- sapply(unique(oVec), function(x) sum(oVec==x))
    ct2 <- rep(defaultCellValue, length(SPG)) #blank vector of 0's as defauls
    ct2[unique(oVec)] <- ct
    
    #turn this into a 
    SPG.df <- SpatialGridDataFrame(SPG,  data=data.frame(polygonCount=ct2))
    
    return(SPG.df)
  }
  
  pixelCount
}

#to remove NAs
rm_na <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
