#This script simulates wastewater data by smoothing hospitalization data, then applying noise scaled by the state of the system

library(pacman)
p_load(tidyverse,deSolve,janitor,lubridate,RcppRoll,conflicted)

conflict_prefer("filter", "dplyr")

source("functions/simulation_functions.R")
################################
#Read data
hosp_dat <- read_csv("input/county_hosp.csv") %>%
  mutate(measure_date=mdy(measure_date))

#Subsetting to Larimer county, linear interpolating missing values, calculate rolling average - we tried a few until getting a smoothed series
smooth_hosp <- hosp_dat %>%
  arrange(measure_date) %>%
  filter(county==8069) %>% #Larimer county
  distinct() %>%
  fit_approx() %>%
  drop_na(hosp) %>%
  mutate(hosp_ma=roll_mean(roll_mean(hosp,n=21,fill=NA),n=21,fill=NA),
         week=floor(as.numeric(measure_date - min(measure_date))/7)) %>%
  drop_na(hosp) 


#Read in the ww data 
ww_data_1 <- read_csv("input/shared/1_SARS-CoV-2_Wastewater_Data_ 2023-04-24 .csv")

ww_data <- ww_data_1 %>%
  filter(wwtp_name == "Fort Collins - Drake") %>%
  dplyr::select(wwtp_name,sample_collect_date,pcr_target_avg_conc) %>%
  mutate(sample_collect_date=mdy(sample_collect_date),
         conc = pcr_target_avg_conc,
         rolling = roll_mean(conc,n=14,fill = NA),
         diff_norm = (conc - rolling)/rolling,
         diff = (conc - rolling)) %>%
  drop_na(pcr_target_avg_conc,diff) %>%
  filter(!is.infinite(diff))

stdev = sd(ww_data$diff)

#plotting ww and hosp data to check agreement
ggplot() +
  geom_line(data=smooth_hosp,aes(x=measure_date,y=scale(hosp)),color="red") +
  geom_line(data=ww_data,aes(x=sample_collect_date,y=scale(conc)),color="blue") +
  labs(x=NULL,y="Scaled ww conc. and hospitalizations",subtitle = "Fort Collins - Drake; Larimer County") +
  theme_bw(base_size = 14)

ggsave("outputs/ww_hosp_compare.png",width = 4.5,height = 4,units = "in")


#####################
#The strategy is to use the smoothed hosp data to generate a synthetic signal and apply noise that assumes properties of the wastewater data
#Why not just smooth wastewater and apply the same strategy? No good reason not to. This was the reason to use the simulation model

ww_data %>%
  ggplot(aes(x=sample_collect_date)) +
  geom_point(aes(y=conc),alpha=.2) +
  geom_line(aes(y=rolling)) +
  labs(x=NULL,y="WW Concentrations",subtitle="WW Concentration data + 14 day rolling average") +
  theme_bw(base_size = 14)

ggsave("outputs/ww_conc_14ma.png",width = 4.5,height = 4,units = "in")


ww_data %>%
  ggplot(aes(x=sample_collect_date,y=diff)) +
  geom_point() +
  labs(x=NULL,y="WW Raw Deviations") +
  theme_bw(base_size = 14)

ggsave("outputs/ww_raw_deviations.png",width = 4.5,height = 4,units = "in")


ww_data %>%
  ggplot(aes(x=sample_collect_date,y=diff_norm)) +
  geom_point() +
  labs(x=NULL,y="WW Normalized Deviations") +
  theme_bw(base_size = 14)

ggsave("outputs/ww_normalized_deviations.png",width = 4.5,height = 4,units = "in")

ww_data %>%
  ggplot(aes(x=dplyr::lag(diff_norm),y=diff_norm)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x="Lagged WW Norm Dev.",y="WW Norm Dev") +
  theme_bw(base_size = 14)

ggsave("outputs/ww_serial_deviations.png",width = 4.5,height = 4,units = "in")




#Function to generate errors like observed in the ww data + add serial correlation like in the ww data
gen_error <- function(x,n){
  set.seed(20)
  corr_coef = coef(lm(x ~ dplyr::lag(x)))[2] #Estimating serial correlation coefficient
  diff_ecdf <- ecdf(x) 
  u=c(1:n)
  u[1]=quantile(diff_ecdf,runif(1), names = FALSE)
  for(i in 2:n){
    u[i] = corr_coef*u[i-1] + quantile(diff_ecdf,runif(1), names = FALSE)
  }
  return(u)
}


#Gen synthetic data
set.seed(20)
sim_dat <- smooth_hosp %>%
  inner_join(select(ww_data,measure_date=sample_collect_date,conc,diff_norm),by="measure_date") %>%
  mutate(hosp_error = hosp_ma*gen_error(diff_norm,nrow(.)),
         hosp_pred = hosp_ma + hosp_error,
         hosp_pred = ifelse(hosp_pred<0,0,hosp_pred))

#Plot to confirm general patterns are similar
sim_dat %>%
  select(measure_date,hosp_pred,conc) %>%
  mutate(across(c(hosp_pred,conc),scale)) %>%
  pivot_longer(-measure_date,names_to = "name",values_to = "value") %>% 
  ggplot() +
  geom_line(aes(x=measure_date,y=value,color=name)) +
  scale_color_discrete(name=NULL) +
  labs(x=NULL,y="Scaled WW conc. + Sim hosp") +
  theme_bw(base_size = 14) +
  theme(legend.position = c(.8,.8),
        legend.background = element_blank())

ggsave("outputs/ww_sim_hosp.png",width = 4.5,height = 4,units = "in")



