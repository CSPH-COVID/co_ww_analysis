#This script reads in and processes raw wastewater data
##########################

library(pacman)
p_load(tidyverse,lubridate,conflicted,bsts)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")

#############################
ww_data_raw <- read_csv("input/shared/1_SARS-CoV-2_Wastewater_Data_2023-04-24.csv")

ww_data_core <- read_csv("input/shared/1_SARS-CoV-2_Wastewater_Data_2023-04-24.csv") %>%
  mutate(measure_date=as_date(mdy(sample_collect_date))) %>%
  select(wwtp_name,measure_date,pcr_target_avg_conc,pcr_target_std_error,quality_flag,flow_rate,population_served,
         pcr_target_below_lod,hum_frac_mic_conc,hum_frac_target_mic)

write_csv(ww_data_core,"cache/ww_data_core.csv")

#Read in ww data
ww_data <- read_csv("input/ww_raw.csv")

#############################################
#Check that all utilities belong to a region
# distinct(ww_data,utility,region) %>%
#   arrange(region,utility) %>%
#   View()

#Check if new utilities exist in the dataset
geo_link_check <- geo_link %>%
  right_join(tibble(utility=unique(ww_raw$utility)))

if(any(is.na(geo_link_check$region))){
  message(str_c("No region or population data for the following utilities: ",str_c(geo_link_check$utility[is.na(geo_link_check$region)],collapse = ", ")))
  write_csv(geo_link_check,"input/geo_link_current.csv")
}


