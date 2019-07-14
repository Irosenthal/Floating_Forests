#' ##############################
#' Summarize user behavior over
#' the course of the project
#' ##############################


#load libraries
library(tidyverse)
library(readr)
library(lubridate)
library(tsibble)
library(ggplot2)

#the relevant directories
read_dir <- "../../../data/relaunch_data/level_0/"
write_fig_dir <-  "../../../figures/relaunch_viz/"

#load the data and filter to relevant
#worflow and columns
ff_data <- readRDS(str_c(read_dir, "floating_forests_classifications.rds")) %>%
  select(classification_id, user_name, user_id, started_at) %>%
  mutate(started_at =  parse_date_time(started_at, orders = "ymdHMS")) %>%
  filter(!is.na(started_at)) #%>%
#  filter(!str_detect(user_name, "not-logged-in"))

#make data into time aware tsibble
ff_data_tsbl <- ff_data %>% 
  as_tsibble(key = classification_id, index = started_at)

#get daily user summary stats
user_summary <- ff_data_tsbl %>%
  
  #first, get summary per user per week
  group_by(user_name) %>%
  index_by(year_week = as.Date(yearweek(started_at))) %>%
  summarize(classifications = n()) %>%
  ungroup() %>%
  as_tibble() %>%
  
  #now let's clean it up to make some nice order to 
  #users for the y-axis
  group_by(user_name) %>%
  mutate(min_date = min(year_week)) %>%
  ungroup() %>%
  arrange(user_name) %>%
  mutate(ord = -1*rank(min_date, ties.method = "first")) %>%
  group_by(user_name) %>%
  mutate(ord = max(ord)) %>%
  ungroup() %>%
  mutate(ord = rank(ord)) %>%
  arrange(year_week) %>%
  
  #if you sparate logged in and not logged in
  mutate(is_logged_user = ifelse(str_detect(user_name, "not-logged-in"), "Not Logged In", "Logged In")) %>%
  group_by(is_logged_user) %>%
  mutate(grouped_ord = rank(ord)) %>%
  ungroup()
  

#plot!
ggplot(user_summary,
       aes(x = ord, y = year_week, size = classifications)) +
  geom_point(alpha = 0.5) +
  coord_flip() +
  theme_bw(base_size = 14) +
  theme(axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(size = guide_legend("Classifications\nper week"))

ggsave(str_c(write_fig_dir, "user_survivorship.jpg"))


#see logged in and not logged in
ggplot(user_summary,
       aes(x = grouped_ord, y = year_week, size = classifications)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~is_logged_user) +
  coord_flip() +
  theme_bw(base_size = 14) +
  theme(axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(size = guide_legend("Classifications\nper week"))

ggsave(str_c(write_fig_dir, "user_survivorship_grouped.jpg"), width = 10)



#see logged in and not logged in
ggplot(user_summary %>% filter(is_logged_user == "Logged In"),
       aes(x = grouped_ord, y = year_week, size = classifications)) +
  geom_point(alpha = 0.5) +
  coord_flip() +
  theme_bw(base_size = 14) +
  theme(axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(size = guide_legend("Classifications\nper week"))

ggsave(str_c(write_fig_dir, "user_survivorship_logged_in.jpg"))
