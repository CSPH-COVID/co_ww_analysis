#This script initializes the project

#loads or install packages
library(pacman) 
p_load(tidyverse,sf,lubridate,bsts,broom,scales,conflicted,progress,patchwork,Hmisc)

#Resolves conflicts between functions from different packages
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("first", "dplyr")
conflict_prefer("last", "dplyr")
conflict_prefer("summarize", "dplyr")

#Load helper functions
source("functions/helper_functions.R")

#ensure all directories exit
folder_setup()
###################################
#Process data
source("code/ww_data_prep.R")

#Run model and classify trends based on current data
source("code/prod_bsts_lm_utility.R")

#Generate plots
source("code/state_region_plots.R")

#Run model on all points in history for internal review
#source("code/bsts_lm_class_utility_test.R")
