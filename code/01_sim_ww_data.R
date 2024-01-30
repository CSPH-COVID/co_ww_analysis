#This script simulates wastewater data by smoothing hospitalization data, then applying noise scaled by the state of the system

library(pacman)
p_load(tidyverse,lubridate,RcppRoll,conflicted,scales,patchwork)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

source("functions/simulation_functions.R")
################################
#Read in the ww data 
ww_data <- read_csv("cache/ww_data_core.csv") %>%
  filter(wwtp_name == "Fort Collins - Drake") %>%
  select(wwtp_name,measure_date,pcr_target_avg_conc) %>%
  mutate(conc = pcr_target_avg_conc,
         rolling = roll_mean(conc,n=14,fill = NA),
         diff_norm = (conc - rolling)/rolling,
         diff = (conc - rolling)) %>%
  drop_na(pcr_target_avg_conc,diff) %>%
  filter(!is.infinite(diff))


#plotting ww data
f1p1 <- ggplot() +
  #geom_line(data=smooth_hosp,aes(x=measure_date,y=scale(hosp)),color="red") +
  geom_point(data=ww_data,aes(x=measure_date,y=conc),alpha=1) +
  geom_line(data=ww_data,aes(x=measure_date,y=rolling),alpha=1,size=1,color="blue") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  scale_x_date(date_labels = "%Y %b") +
  labs(x=NULL,y="WW Concentrations") +
  theme_bw(base_size = 12)


f1p2 <- ww_data %>%
  ggplot(aes(x=measure_date,y=diff_norm)) +
  geom_point() +
  scale_x_date(date_breaks = "year",date_labels = "%Y") +
  labs(x=NULL,y="WW Normalized Dev.") +
  theme_bw(base_size = 12)


f1p3 <- ww_data %>%
  ggplot(aes(x=dplyr::lag(diff_norm),y=diff_norm)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x="Lagged WW Norm Dev.",y="WW Norm Dev.") +
  theme_bw(base_size = 12)

f1p1 / (f1p2 | f1p3) + 
  plot_annotation(tag_levels = 'a',tag_suffix = ')')

ggsave("outputs/ww_drake_example.png",width = 6,height = 6,units = "in")


sc_model <- lm(diff_norm ~ lag(diff_norm),data = ww_data) 

summary(sc_model)

confint(sc_model)

#Gen synthetic data
sim_dat <- ww_data %>%
  mutate(sim_error = rolling*gen_error(diff_norm,nrow(.),seed=22)[[1]],
         sim_pred = rolling + sim_error)

gen_error(ww_data$diff_norm,2,seed=22)[-1]

summary(sim_dat$sim_pred)

#Plot to confirm general patterns are similar
sim_dat %>%
  select(measure_date,sim_pred,conc) %>%
  mutate(across(c(sim_pred,conc),scale)) %>%
  pivot_longer(-measure_date,names_to = "name",values_to = "value") %>% 
  ggplot() +
  geom_line(aes(x=measure_date,y=value,color=name)) +
  scale_color_discrete(name=NULL) +
  labs(x=NULL,y="Scaled WW conc. + Sim hosp") +
  theme_bw(base_size = 14) +
  theme(legend.position = c(.8,.8),
        legend.background = element_blank())

ggsave("outputs/ww_sim_hosp.png",width = 4.5,height = 4,units = "in")

write_csv(sim_dat,"cache/simulated_data.csv")

