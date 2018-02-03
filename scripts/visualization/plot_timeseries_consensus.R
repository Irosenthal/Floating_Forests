#'#############
#'
#'  File to make plot of timeseries
#'  from Isaac's classification data
#'  
#'#############


##Load libraries and data
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggmap)
library(viridis)

mcc_data <- readRDS("../../data/output/sensitivity_analysis/full_CA_mcc.rds")


#add temporal data
mcc_data <- mcc_data %>%
  mutate(DATE_ACQUIRED = parse_date_time(DATE_ACQUIRED, orders="ymd"),
         quarter = quarter(DATE_ACQUIRED, with_year=TRUE, fiscal_start=12)) %>%
  mutate(year = year(DATE_ACQUIRED),
         month = month(DATE_ACQUIRED),
         season = gsub("[1,2][0,9,9][0-9][0-9].", "", quarter)) %>%
  mutate(season = forcats::fct_recode(season, Winter = "1", 
                                      Spring = "2",
                                      Summer = "3",
                                      Fall = "4"),
         path_row = paste(WRS_PATH, WRS_ROW, sep="_"))

head(mcc_data)

get_quarterly_value <- . %>%
  group_by(path_row, DATE_ACQUIRED, users, quarter, season) %>%
  summarize(userKelpPixels = sum(userKelpPixels, na.rm=TRUE),
            nImages = n_distinct(zooniverse_ID)) %>%
  ungroup() %>%
  group_by(users, quarter,  path_row, season) %>%
  summarize(userKelpPixels = max(userKelpPixels, na.rm=T),
            nImages = nImages[which(userKelpPixels == max(userKelpPixels, na.rm=T))]) %>%
  ungroup()

mcc_data_11_pathrow_quarterly <- mcc_data %>% 
  filter(UTM_ZONE==11, path_row == "41_36") %>%
  filter(users<=15) %>%
  get_quarterly_value


mcc_data_11 <- mcc_data_11_pathrow_quarterly %>%
  group_by(users, quarter,  season) %>%
  summarize(totalKelp = sum(userKelpPixels, na.rm=T),
            nImages = sum(nImages),
            nPathrow = n_distinct(path_row)) %>%
  ungroup() %>%
  filter(quarter<2013)

#Effort
qplot(quarter, nImages, data=mcc_data_11) +
  geom_line() +
  facet_wrap(~season)

qplot(quarter, nPathrow, data=mcc_data_11) +
  geom_line() +
  facet_wrap(~season)

plot_format <-  list(
  aes(x=quarter, y=totalKelp, color=factor(users), group=users),
  geom_line(),
  scale_color_viridis(discrete=TRUE, guide=guide_legend(title="User\nThreshold")),
  theme_bw(base_size=17),
  xlab("Date"),
  ylab("Kelp Pixels") ,
    scale_x_continuous(breaks = seq(1984, 2012, by=2)),
  theme(axis.text.x = element_text(hjust=1, angle=45)))

ggplot(mcc_data_11) +
  plot_format
 
ggplot(mcc_data_11 %>% filter(users >3) %>% filter(users<10) )+
  plot_format 

ggplot(mcc_data_11 %>% filter(users >3) %>% filter(users<10) )+
  plot_format  +
  facet_wrap(~season)
