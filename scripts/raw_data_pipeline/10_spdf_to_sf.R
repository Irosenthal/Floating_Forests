library(maptools)
library(sp)
library(sf)
library(readr)
library(tidyr)
library(cleangeo)

#convert all SPDFs to SFs

#one per UTM zone
#final data will include subject ID, user ID, and geoms

#raw data
classifications <- read_rds("../../data/output/raw_data_pipeline/sp_classifications_df_zone_10.rds")
tiles <- classifications$SPDF

#convert all UTM to LL
tiles_LL <- map(tiles, spTransform, CRS("+proj=longlat +datum=WGS84"))

#try cleaning data to remove orphaned holes

#create a report of all issues
report <- map(tiles_LL, clgeo_CollectionReport)
#unlist
report <- do.call(rbind.data.frame, report)
#add subjects for tracking down these issues
report$subject <- row.names(report)summary <- clgeo_SummaryReport(report)
#see all subjects with orphaned holes
issues <- report[report$valid == FALSE,] %>%
  filter(issue_type == "ORPHANED_HOLE")

#Should I drop these subjects entirely? 
#try to keep digging to find the actual problematic classifications? 
#in either case, step 1 is isolate the problematic subjects. 




#now convert all to SFs
#This fails - orphaned holes. see above for cleaning
tiles_SF <- map(tiles_LL, st_as_sf, "sf")


#now unlist into one big DF for each UTM zone
sf_master <- do.call(rbind.data.frame, map_test_sf) %>%
  select(subject_zooniverse_id, user_name)

#save
saveRDS(sf_master, "../../data/output/raw_data_pipeline/zone_10_classifications.rds")
