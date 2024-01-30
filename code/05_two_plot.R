#This script plots the current state and trend on the two dimensional figure

library(pacman)
p_load(tidyverse,lubridate,conflicted,arrow,janitor,patchwork,EpiWeek)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::first)

ww_bsts_all <- read_csv("cache/bsts_out_all.csv") 

ww_bsts_all %>%
  filter(utility=="Fort Collins - Boxelder",series=="sars") %>%
  ggplot(aes(x=measure_date,y=state_pr)) +
  geom_line()

sum_plot <- ww_bsts_all %>%
  filter(series=="sars") %>%
  mutate(epi_week = epiweek(measure_date),
         epi_year = epiyear(measure_date)) %>% 
  group_by(utility,epi_week,epi_year) %>%
  summarize(across(c(state_pr,state_slope_slope),~mean(.,na.rm = T))) %>%
  ungroup() 



#xrange=c(min(sum_plot$state_slope_slope,na.rm = T),max(sum_plot$state_slope_slope,na.rm = T))
xrange=c(-.2,.2)
#yrange=c(min(sum_plot$state_pr,na.rm = T),max(sum_plot$state_pr,na.rm = T))
yrange=c(0,1)
bg_color=tibble(xmin=c(0,xrange[1],0,xrange[1]),
                xmax=c(xrange[2],0,xrange[2],0),
                ymin=c(.5,.5,0,0),
                ymax=c(1,1,.5,.5),
                clrs=str_c("#",c("e76f51","e9c46a","e9c46a","1495CC")))

snap_dates <- tibble(eweek=c(20,50,30,10),
                     eyear=c(2021,2021,2022,2023))

ddf <- snap_dates[1,]

plots <- group_split(snap_dates,row_number()) %>%
  map(function(ddf){
    plot_dat <- sum_plot %>% 
      filter(epi_week==ddf$eweek,epi_year==ddf$eyear) %>%
      reverse_epiweek_lookup()
    
    p_out <- ggplot() +
      geom_rect(data = bg_color,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=clrs),alpha=.2,show.legend = FALSE) +
      geom_point(data=plot_dat,aes(x=state_slope_slope,y=state_pr)) +
      scale_fill_identity() +
      geom_vline(xintercept = 0,linetype="dashed") +
      geom_hline(yintercept = 0.5,linetype="dashed") +
      annotate(geom="text",x=.2,y=.1,label="Low but increasing",hjust="right") +
      annotate(geom="text",x=.2,y=.9,label="High and increasing",hjust="right") +
      annotate(geom="text",x=-.2,y=.1,label="Low and decreasing",hjust="left") +
      annotate(geom="text",x=-.2,y=.9,label="High but decreasing",hjust="left") +
      xlim(xrange) +
      ylim(yrange) +
      labs(x="Trend",y="Index",subtitle = plot_dat$measure_date[1]) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
            axis.text.x = element_blank(),axis.text.y = element_blank(),plot.subtitle = element_text(hjust=0.5))
    
    return(p_out)
  })

(plots[[1]] + plots[[2]]) / (plots[[3]] + plots[[4]]) +
  plot_annotation(tag_levels = "a",tag_suffix = ")")

ggsave("outputs/two_plot.pdf",width = 11,height = 8,units = "in")  


