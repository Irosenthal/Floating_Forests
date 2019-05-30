#'-----------------------------
#' Script that chooses images for the zoom interface
#' and downloads them, as well as generating
#' metadata
#'-----------------------------

setwd(here::here())
library(sf)
library(dplyr)
library(purrr)


#First load the good subjects table
good_subj <- readRDS("./data/relaunch_data/level_1/merged_sf_tiles.Rds")
all_classifications <- readRDS("./data/relaunch_data/level_0/floating_forests_classifications.rds")

#select 100 subjects using the tau seed
set.seed(pi*2)
zoom_subj <- good_subj[[2]] %>%
  filter(threshold==4) %>%
  filter(!st_is_empty(.)) %>%
  sample_n(300)

#make sure it's not been flagged a bad image

#make a more managable classifications table
all_classifications <- all_classifications %>%
  filter(subject_ids %in% zoom_subj$subject_id) 

#determine which subjects have been flagged as 
#bad images by x number of people
all_classifications_bad <- all_classifications %>%
  group_by(subject_ids) %>%
  summarize(is_bad = map_df(annotations, jsonlite::fromJSON) %>%
              pull(value) %>%
              map_lgl(~ "Bad Image" %in% .) %>%
              sum)

#now, filter out bad ids
#and resample
set.seed(pi*2)
zoom_subj_filtered  <- left_join(zoom_subj, 
                        all_classifications_bad %>% 
                          rename(subject_id = subject_ids) %>%
                          mutate(subject_id = as.character(subject_id))) %>%
  filter(is_bad==0) %>% 
  sample_n(100) #the final 100!

#get the original consensus classifications and write out              
zoom_consensus <- good_subj[[2]] %>%
  filter(subject_id %in% zoom_subj_filtered$subject_id)

saveRDS(zoom_consensus, "./data/zoom/for_launch/zoom_consensus_from_ff_falklands.Rds")

#load the subject metadata table
subj_table <- readRDS("./data/relaunch_data/level_0/ff_relaunch_subjects.rds") %>%
  filter(subject_id %in% zoom_subj_filtered$subject_id) %>%
  mutate(`#filename` = basename(locations)) %>%
  select(subject_id, latitude, longitude, acquired_date, acquired_time,
         `!scene`, map_link, sensor_id, `#utm_zone`, spacecraft, locations,
         `#filename`) %>%
  rename(falklands_subject_id = subject_id)

#write the appropriate metadata out
write.csv(subj_table %>% select(-locations), "./data/zoom/for_launch/manifest.csv")

#download the images
get_ff_image_for_zoom <- function(subj_url){
  download.file(subj_url, 
                paste0("./data/zoom/for_launch/", basename(subj_url)), mode = 'wb')
}

purrr::walk(subj_table$locations, get_ff_image_for_zoom)
