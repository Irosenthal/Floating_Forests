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

#cal data
SB_cal <- read_csv( "../../data/calibration/SB_cal" )
CCA_cal <- read_csv("../../data/calibration/CCA_cal")

########
#use sp_classifications_df from sensitivity_analysis.R
sp_classifications_df <- readRDS("../../data/products/sp_classifications_df_SB_cal.rds")#.rds for testing 1 scene
#sr <- "+proj=longlat +datum=WGS84"
sr <- "+proj=utm +zone=10 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

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



overlap <- tryCatch(!is.null(crop(caKelp.spoints,extent(rastLayer))), error=function(e) return(FALSE)) 
#caKelp.raster <- ifelse(caKelp.spoints_crop = !NULL, rasterize(caKelp.spoints_crop, r, fun="count"), NA)


#if(extent(rastLayer is in calibration data do this stuff, else NA))

calculate_J <- function(arow){
    #each row is one of the dataframes in the scene, aka a tile
    x <- arow
    print(x)
    #convert the spdf for the tile to a raster
    r <- raster(crs=sp_classifications_df[[x,1]]@proj4string, ext=extent(sp_classifications_df[[x,1]]), 
                ncols = 400, nrows=364)
    rastLayer <- rasterize(SpatialPolygons(sp_classifications_df[[x,1]]@polygons), r, fun="count")

    #crop calibration data to the tile extent
    #find the correct calibration data:
    cal_df <- CCA_cal[,c(1:2, which(colnames(CCA_cal)== as.character(sp_classifications_df[[x,1]]@data$DATE_ACQUIRED[1])))]
    sr <- "+proj=longlat +datum=WGS84"
    #"+proj=utm +zone=10 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
    caKelp.spoints <- SpatialPoints(cal_df[,2:1], 
                                    proj4string=CRS(sr))
    
    caKelp.spoints <- spTransform(caKelp.spoints, "+proj=utm +zone=10 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    
    caKelp.spoints_crop <- crop(caKelp.spoints, extent((sp_classifications_df[[x,1]])))
    
    if(length(caKelp.spoints_crop) > 0){
    caKelp.raster <- rasterize(caKelp.spoints_crop, r, fun="count")
    
    #convert it to a raster
    #use mask to find all pixels present in both user and calibration data
    r3 <- mask(rastLayer, caKelp.spoints_crop)
    #create a dataframe that has how many kelp pixels each user threshold contains, 
    #as well as how many of these overlap the calibration set
    kelpPixels <- data.frame(users=1:15,
                             zooniverse_ID = as.character(sp_classifications_df[[x,1]][[3,]][1]),
                             userKelpPixels = getKelpPixelsFromRasterV(rastLayer, 1:15),
                             goodKelpPixels = getKelpPixelsFromRasterV(r3, 1:15))
    
    #add columns for the number of pixels in the calibration dataset as well as the total number of pixels in the image  
    kelpPixels <- kelpPixels %>%
        mutate(realKelpPixels = getKelpPixelsFromRaster(caKelp.raster)) %>%
        mutate(totalPixels = getTotalPixelsFromRaster(rastLayer))
    
    
    #add columns for all needed componants of J
    kelpPixels <- kelpPixels %>%
        mutate(TP = goodKelpPixels) %>%
        mutate(TN = totalPixels - userKelpPixels + (realKelpPixels - goodKelpPixels)) %>%
        mutate(FP = userKelpPixels - goodKelpPixels) %>%
        mutate(FN = realKelpPixels - goodKelpPixels)
    
    #Now that we have all the componants, we can calclate J!
    kelpPixels <- kelpPixels %>%
        mutate(J = (TP/(TP + FN)) + 
                   (TN/(TN + FP)) -1)
    #we can also calculate mcc:
    kelpPixels <- kelpPixels %>%
      mutate(sum1 = sqrt(TP + FP)) %>% 
      mutate(sum2 = sqrt(TP + FN)) %>%
      mutate(sum3 = sqrt(TN + FP)) %>%
      mutate(sum4 = sqrt(TN + FN)) %>%
      mutate(denom = as.double(sum1*sum2*sum3*sum4))
    
    kelpPixels <- kelpPixels %>%
      mutate(mcc = ((TP*TN)-(FP*FN)) / (denom) )
    

    
    return(as.data.frame(kelpPixels))
    } 
    else{
        
        
        kelpPixels <- data.frame(users=1:15, 
                                 zooniverse_ID = as.character(sp_classifications_df[[x,1]][[3,]][1]),
                                 userKelpPixels = NA,
                                 goodKelpPixels = NA)
        
        #add columns for the number of pixels in the calibration dataset as well as the total number of pixels in the image  
        kelpPixels <- kelpPixels %>%
            mutate(realKelpPixels = NA) %>%
            mutate(totalPixels = NA)
        
        
        #add columns for all needed componants of J
        kelpPixels <- kelpPixels %>%
            mutate(TP = NA) %>%
            mutate(TN = NA) %>%
            mutate(FP = NA) %>%
            mutate(FN = NA)
        
        #Now that we have all the componants, we can calclate J!
        kelpPixels <- kelpPixels %>%
            mutate(J = NA)
        #or mcc
        kelpPixels <- kelpPixels %>%
          mutate(sum1 = NA) %>% 
          mutate(sum2 = NA) %>%
          mutate(sum3 = NA) %>%
          mutate(sum4 = NA) %>%
          mutate(denom = NA)
        
        kelpPixels <- kelpPixels %>%
          mutate(mcc = NA)
        
            }
}

J_out <- sapply(1:nrow(sp_classifications_df), calculate_J)
#transpose it
data_long <- as.data.frame(t(J_out))

#unnest
data_long <- data_long %>%
  unnest()

write_csv(data_long, "../../data/products/CCA_calibrations.csv")

########
#need to make a nested spdf for the scenes we care about INSTEAD OF the test one we've been using.
#for looking up correct cal data
sp_classifications_df[[x,1]]@data$DATE_ACQUIRED[1] #is the date and should correspond to each column in my cal file

19840418


#find the correct calibration data:
colnames(SB_cal)[3] #is the date itself
#sp_classifications_df[[x,1]]@data$DATE_ACQUIRED needs to match up with colnames(SB_cal)[3]
#make a spdf of the coords (1st 2 columns) and the data (correct date column) and then continue from there

#how about:
cal_df <- SB_cal[,c(1:2, which(colnames(SB_cal) == as.character(sp_classifications_df[[x,1]]@data$DATE_ACQUIRED[1])))]
sp_classifications_df[[305,1]]@data$DATE_ACQUIRED[1]

sr <- "+proj=longlat +datum=WGS84"
#"+proj=utm +zone=10 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
cal_df.spoints <- SpatialPoints(cal_df[,2:1], 
                                proj4string=CRS(sr))
