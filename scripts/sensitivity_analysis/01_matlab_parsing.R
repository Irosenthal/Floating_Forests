####script for processing matlab files
caKelp <- readMat("../../data/calibration/kelp_043035_7_24_1999.mat")
caKelp <- caKelp$kelp[which(caKelp$kelp[,3]>0),]


morekelp <- readMat("../../data/calibration/Santa_Barbara.mat")

SB_coords_lat <- as.vector(morekelp[[1]][,1])
SB_coords_lon <- as.vector(morekelp[[1]][,2])
dates <- morekelp[[2]]
kelp <- as.data.frame(morekelp[[3]])
df <- data.frame(matrix(ncol = length(dates), nrow = 0))
kelp <- rbind(df, kelp)
#format dates from cal data
dates_df <- data.frame(date = t(dates)) %>%
    mutate(date_cal  = ymd(date))
colnames(kelp) <- (dates_df$date_cal)
coords<- data.frame(SB_coords_lat, SB_coords_lon)
SB_cal <- cbind(coords, kelp)
write_csv(SB_cal, "../../data/calibration/SB_cal" )
test_cal <- SB_cal[,1:3]
#use this snippit later
filtered_SB_cal <- SB_cal[3][which(test_cal[3]>0),]

#######first find all the scenes
#make a df of dates and scenes
scene_meta <- read_csv("../../data/products/scene_metadata.csv")

LC80400372013110LGN01 

#create a column of wrs for the join
scene_meta <- scene_meta %>%
  mutate(wrs = as.numeric(str_sub(scene, start = 4, end = -13))) 

#filter all data to get just the ones we need - for santa barbara: 042036

SB_scene_meta <- scene_meta %>%
  filter(wrs == 042036)

#format a date column
SB_scene_meta <- SB_scene_meta %>%
  mutate(date_cal = as_date(DATE_ACQUIRED))

#format dates from cal data
dates_df <- data.frame(date = t(dates)) %>%
mutate(date_cal  = ymd(date))
#do a test join to see whats not there

test_date_join <- left_join(dates_df, SB_scene_meta, by = "date_cal" )

####################################
####################################
####################################
####################################
#do it again for central CA
####################################
####################################
####################################



central_kelp <- readMat("../data/calibration/Central_CA.mat")

CCA_coords_lat <- as.vector(central_kelp[[1]][,1])
CCA_coords_lon <- as.vector(central_kelp[[1]][,2])
dates <- central_kelp[[2]]
kelp <- as.data.frame(central_kelp[[3]])
df <- data.frame(matrix(ncol = length(dates), nrow = 0))
kelp <- rbind(df, kelp)
#format dates from cal data
dates_df <- data.frame(date = t(dates)) %>%
    mutate(date_cal  = ymd(date))
colnames(kelp) <- (dates_df$date_cal)
coords<- data.frame(CCA_coords_lat, CCA_coords_lon)
CCA_cal <- cbind(coords, kelp)
write_csv(CCA_cal, "../../data/calibration/CCA_cal" )


#######first find all the scenes
#make a df of dates and scenes
scene_meta <- read_csv("../../data/products/scene_metadata.csv")

LC80400372013110LGN01 

#create a column of wrs for the join
scene_meta <- scene_meta %>%
    mutate(wrs = as.numeric(str_sub(scene, start = 4, end = -13))) 

#filter all data to get just the ones we need - for santa barbara: 042036

SB_scene_meta <- scene_meta %>%
    filter(wrs == 042036)

#format a date column
SB_scene_meta <- SB_scene_meta %>%
    mutate(date_cal = as_date(DATE_ACQUIRED))

#format dates from cal data
dates_df <- data.frame(date = t(dates)) %>%
    mutate(date_cal  = ymd(date))
#do a test join to see whats not there

test_date_join <- left_join(dates_df, SB_scene_meta, by = "date_cal" )
         