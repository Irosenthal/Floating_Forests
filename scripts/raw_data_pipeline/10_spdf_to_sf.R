library(maptools)
library(sp)
library(sf)
library(readr)
library(tidyr)
library(cleangeo)
library(purrr)
#convert all SPDFs to SFs

#one per UTM zone
#final data will include subject ID, user ID, and geoms

#raw data
classifications <- read_rds("../../data/output/raw_data_pipeline/sp_classifications_df_zone_55.rds")
tiles <- classifications$SPDF


#convert all UTM to LL
tiles_LL <- map(tiles, spTransform, CRS("+proj=longlat +datum=WGS84"))


  ####cleaning data to remove orphaned holes

#create a report of all issues
report <- map(tiles_LL, clgeo_CollectionReport)
#unlist
report <- do.call(rbind.data.frame, report)
#add subject IDS to help track down problematic subjects
report$subject <- row.names(report)
#sanity check
summary <- clgeo_SummaryReport(report)
#see all subjects with orphaned holes
issues <- report[report$valid == FALSE,] %>%
  dplyr::filter(issue_type == "ORPHANED_HOLE")

#Should I drop these subjects entirely? - YES (5/15 talked to Jarrett)
#to drop, filter out these subject IDs

#clean up subject IDs for filtering
issues$subject <- str_replace(issues$subject,"\\..*","")


#use those issue subjects to drop bad subjects
bad_sub_idx <- map_lgl(tiles_LL, ~.x@data$subject_zooniverse_id[1] %in% issues$subject)
classifications_clean <- tiles_LL
classifications_clean[bad_sub_idx] <- NULL


#now convert all to SFs
tiles_SF <- map(classifications_clean, st_as_sf, "sf")


#now unlist into one big DF for each UTM zone
sf_master <- do.call(rbind.data.frame, tiles_SF) %>%
  dplyr::select(subject_zooniverse_id, user_name)

#save
saveRDS(sf_master, "../../data/output/raw_data_pipeline/zone_55_classifications.rds")
