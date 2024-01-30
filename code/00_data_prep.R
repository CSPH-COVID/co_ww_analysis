#This script reads in and processes raw wastewater data
##########################

library(pacman)
p_load(tidyverse,lubridate,conflicted)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")

#############################
ww_data_core <- read_csv("input/shared/1_SARS-CoV-2_Wastewater_Data_2023-04-24.csv") %>%
  mutate(measure_date=as_date(mdy(sample_collect_date))) %>%
  select(wwtp_name,measure_date,pcr_target_avg_conc,pcr_target_std_error,quality_flag,flow_rate,population_served,
         pcr_target_below_lod,hum_frac_mic_conc,hum_frac_target_mic)

write_csv(ww_data_core,"cache/ww_data_core.csv")



