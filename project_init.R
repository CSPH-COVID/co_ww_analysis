#This script initializes the project

#loads or install packages
library(pacman) 
p_load(tidyverse,sf,lubridate,bsts,broom,scales,conflicted,patchwork,Hmisc,janitor)

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

