#Load libraries and data
source("./load_prep_classifications.R")


### Plot one subject ####
plot_one_subject <- function(single_subject){

  akp <- classifications_df %>% filter(zooniverse_id==single_subject)
  akp_map <- get_map(c(min(akp$long), min(akp$lat), max(akp$long), max(akp$lat)),
                   zoom=13)

  ggmap(akp_map) +
    geom_polygon(data = akp, 
                 mapping=aes(x=long, y=lat, group=group,fill=factor(threshold))) +
    xlim(min(akp$long)-0.02, max(akp$long)+0.02) +
    ylim(min(akp$lat)-0.01, max(akp$lat)+0.01) +
    scale_fill_discrete(guide=guide_legend(title="Min. # Users")) +
    xlab("") + ylab("")
}

plot_one_subject("AKP00004zp")