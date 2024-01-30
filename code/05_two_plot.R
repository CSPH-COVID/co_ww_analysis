#This script plots the current state and trend on the two dimensional figure

library(pacman)
p_load(tidyverse,lubridate,conflicted,arrow,janitor)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::first)

ww_bsts_all <- read_parquet("outputs/comp_dat/comp_dat_metro_wastewater_rwhtf_prc.parquet") %>% #read_csv("outputs/all_comp.csv") 
  bind_rows(read_parquet("outputs/comp_dat/comp_dat_broomfield.parquet"))


sum_plot <- ww_bsts_all %>%
  filter(series=="sars") %>%
  mutate(epi_week = epiweek(measure_date),
         epi_year = epiyear(measure_date)) %>% 
  group_by(utility,epi_week,epi_year) %>%
  summarize(across(c(state_pr,state_slope_slope),~mean(.,na.rm = T))) %>%
  ungroup() 

xrange=c(min(sum_plot$state_slope_slope,na.rm = T),max(sum_plot$state_slope_slope,na.rm = T))
#yrange=c(min(sum_plot$state_pr,na.rm = T),max(sum_plot$state_pr,na.rm = T))
yrange=c(-1,1)
bg_color=tibble(xmin=c(0,xrange[1],0,xrange[1]),
                xmax=c(xrange[2],0,xrange[2],0),
                ymin=c(0,0,-1,-1),
                ymax=c(1,1,0,0),
                clrs=str_c("#",c("e76f51","e9c46a","2a9d8f","1495CC")))


ggplot() +
  geom_rect(data = bg_color,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=clrs),alpha=.2,show.legend = FALSE) +
  geom_point(data=sum_plot %>% filter(epi_week==36,epi_year==2021),aes(x=state_slope_slope,y=state_pr)) +
  scale_fill_identity() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  xlim(xrange) +
  ylim(yrange) +
  labs(x="Trend",y="Index") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())
  


