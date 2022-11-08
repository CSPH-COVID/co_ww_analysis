#This script reads in and processes raw wastewater data
##########################
#Not run: 

#Link to regions: Jude made this from data Rachel W. shared
geo_link <- read_csv("input/geo_link.csv")

#WW data from cdphe api
ww_raw <- read_sf("https://opendata.arcgis.com/datasets/c6566d454edb47c680afe839a0b4fc26_0.geojson") %>%
  st_set_geometry(NULL) %>%
  select(measure_date=Date,utility=Utility,sars=SARS_CoV_2_copies_L,new_cases=Number_of_New_COVID19_Cases_by_) %>%
  mutate(measure_date=mdy(measure_date)) 

n_distinct(ww_raw$utility)

ww_raw <- ww_raw %>%
  left_join(geo_link,by = "utility") %>%
  arrange(utility,measure_date) %>%
  drop_na()

write_csv(ww_raw,"input/ww_raw.csv")

#Read in ww data
ww_data <- read_csv("input/ww_raw.csv")

#############################################
#Check that all utilities belong to a region
# distinct(ww_data,utility,region) %>%
#   arrange(region,utility) %>%
#   View()


