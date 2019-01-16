#' ##############################
#' Separate classification, yes/no, 
#' and other workflow data into separate 
#' .Rds files
#' 
#' ##############################


#load libraries
library(tidyverse)
library(readr)
library(lubridate)
library(jsonlite)

read_dir <- "../../../data/relaunch_data/raw_data/"
write_dir <-  "../../../data/relaunch_data/level_0/"


#read relaunch data
ff_relaunch <- read_csv(str_c(read_dir, "floating-forests-classifications.csv"),
                         col_types="icicicdcccccci") %>%
   mutate(created_at = lubridate::parse_date_time(created_at, orders="ymdHMS"))
 
#read subjects and deparse JSON 
ff_relaunch_subjects <- read_csv(str_c(read_dir, "floating-forests-subjects-2018-12-12.csv")) %>% 
  mutate(
    metadata = map(metadata, ~fromJSON(.x) %>% as_tibble),
    locations = map(locations, fromJSON)) %>%  
  unnest(locations) %>%
  mutate(locations = unlist(locations)) %>%  
  unnest(metadata)


# TO deal with data from when we worked on the beta of the site
# ff_relaunch_beta <- ff_relaunch %>%
#   filter(created_at < parse_date_time("12-12-2017", orders="mdy")) %>%
#   filter(created_at > parse_date_time("04-18-2017", orders="mdy")) 

# All data created after relaunch
ff_relaunch <- ff_relaunch %>%
  filter(created_at > parse_date_time("12-12-2017", orders="mdy"))

##
# split into different workflows and deparse the JSON
##

#helpful workflow/functions
#although really only works for
#well behaved JSON
flatten_json <- . %>%
  fromJSON(.) %>% 
  purrr::flatten() %>% 
  map(~ifelse(is.null(.x), NA, .x)) %>%
  as_tibble

#lots of json error checking in here - oy
#the metadata column just keeps changing...
#so this function deals with the many different issues I've found
#and I <3 possibly as it helps me debug and inserts flags
#for later scripts down the pipeline to help me find chokepoints
possibly_deparse <- possibly(function(x) x %>% 
                               fromJSON() %>% 
                               purrr::flatten_df() %>% 
                               #deal with the constantly changing metadata JSON
                               setNames( make.names(names(.), unique = TRUE)) %>%
                               select(-starts_with("X")) %>%
                               mutate_all(.funs = as.character), 
                             
                             #if the above fails, return a no_metadata_info
                             otherwise = tibble(no_metadata_info = TRUE))

#the main deparsing workflow
deparse_json <- . %>%
  mutate(subject_data = map(subject_data, flatten_json),
         metadata = map(metadata, possibly_deparse)) %>%
  unnest(subject_data, metadata) %>%
  select(-starts_with("V"))

#classification workflow
ff_relaunch_classifications <- ff_relaunch %>%
  filter(workflow_id == 2150)  %>% 
  deparse_json()

#swipe workflow only
ff_relaunch_swipe <- ff_relaunch %>%
  filter(workflow_id == 3246)  %>% 
  deparse_json()

#write data as rds to save space
saveRDS(ff_relaunch_subjects, str_c(write_dir, "ff_relaunch_subjects.rds"))
saveRDS(ff_relaunch_classifications, str_c(write_dir, "floating_forests_classifications.rds"))
saveRDS(ff_relaunch_swipe, str_c(write_dir, "floating_forests_swipe.rds"))
