#automation of sensitivity analysis, starting for one scene

#take a scene dataframe
#for each tile
#rasterize user classifications
#rasterize calibration data
#crop calibration data 
#create combined overlay
#create dataframe with:
#the total pixels for each user threshold in user classifications
#the total pixels for each user threshold in the overlaid rasters
#do math for J

########
#use sp_classifications_df from sensitivity_analysis.R
as.data.frame(sp_classifications_df)
#turn calibration data into spdf
caKelp.spoints <- SpatialPoints(caKelp[,2:1], 
                                proj4string=CRS(sr))
#some functions
getKelpPixelsFromRaster <- function(rast, threshold=1){
    length(which(as.matrix(rast)>=threshold))
}

getTotalPixelsFromRaster <- function(rast){
    length(as.matrix(rast))
}

getKelpPixelsFromRasterV <- Vectorize(getKelpPixelsFromRaster, "threshold")
arow <- 5

calculate_J <- function(arow){
    #each row is one of the dataframes in the scene, aka a tile
    x <- arow
    #convert the spdf for the tile to a raster
    r <- raster(crs=sp_classifications_df[[x,1]]@proj4string, ext=extent(unlist(sp_classifications_df[[x,1]])), 
                ncols = 400, nrows=364)
    rastLayer <- rasterize(SpatialPolygons(sp_classifications_df[[x,1]]@polygons), r, fun="count")
    #crop calibration data to the tile extent
    caKelp.spoints_crop <- crop(caKelp.spoints, extent(unlist(sp_classifications_df[[x,1]])))
    #convert it to a raster
    caKelp.raster <- rasterize(caKelp.spoints_crop, r, fun="count")
    #use mask to find all pixels present in both user and calibration data
    r3 <- mask(rastLayer, caKelp.spoints_crop)
    
    #create a dataframe that has how many kelp pixels each user threshold contains, 
    #as well as how many of these overlap the calibration set
    kelpPixels <- data.frame(users=1:15, 
                             userKelpPixels = getKelpPixelsFromRasterV(rastLayer, 1:15),
                             goodKelpPixels = getKelpPixelsFromRasterV(r3, 1:15))
    
    #add columns for the number of pixels in the calibration dataset as well as the total number of pixels in the image  
    kelpPixels <- kelpPixels %>%
        mutate(realKelpPixels = getKelpPixelsFromRaster(caKelp.raster)) %>%
        mutate(totalPixels = getTotalPixelsFromRaster(rastLayer))
    #add columns for all needed componants of J
    kelpPixels <- kelpPixels %>%
        mutate(true_positive = goodKelpPixels) %>%
        mutate(true_negative = totalPixels - userKelpPixels + (realKelpPixels - goodKelpPixels)) %>%
        mutate(false_positive = userKelpPixels - goodKelpPixels) %>%
        mutate(false_negative = realKelpPixels - goodKelpPixels)
    
    #Now that we have all the componants, we can calclate J!
    kelpPixels <- kelpPixels %>%
        mutate(J = (true_positive/(true_positive + false_negative)) + 
                   (true_negative/(true_negative + false_positive)) -1)
    return(kelpPixels)
}

J_out <- sp_classifications_df %>% 
    rowwise() %>%
    do((calculate_J(.)))

J_out <- sp_classifications_df %>%
    lapply(calculate_J)

library(purrr)

J_out <- sp_classifications_df %>%
    map(calculate_J)
