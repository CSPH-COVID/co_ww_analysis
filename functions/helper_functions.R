#Helper functions

#This function builds out the folder structure
folder_setup <- function(){
  folder.list <- c("code",
                   "cache",
                   "input",
                   "outputs")
  
  lapply(folder.list,
      function(x){
        if(!dir.exists(x)){
          dir.create(x,recursive = T)
          message(paste0("The ",x," folder has been created."))
        } else {
          message(paste0("The ",x," folder already exists."))
        }
      })
  
  return(NULL)
}


#####################
#Creating date lookup function for converting from epiweek to date
#df=sum_plot %>% filter(epi_week==20,epi_year==2022)
reverse_epiweek_lookup <- function(df,week_var="epi_week",year_var="epi_year"){
  
  require(dplyr)
  require(lubridate)
  
  temp <- tibble(measure_date=seq.Date(from=as_date("2020-01-01"),to=as_date(today()),by=1)) %>%
    mutate(epi_week=epiweek(measure_date),
           epi_year=epiyear(measure_date)) %>%
    group_by(epi_week,epi_year) %>%
    summarize(measure_date=first(measure_date)) %>%
    ungroup() %>%
    inner_join(df,.,by=c("epi_week","epi_year"))
  
  return(temp)
  
}

