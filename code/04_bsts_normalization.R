# This script tests whether different normalization methods have an impact on the trend classification

library(pacman)
p_load(tidyverse,lubridate,conflicted,bsts,arrow,janitor)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::first)

source("functions/simulation_functions.R")
source("functions/analysis_functions.R")

#Read data
ww_data <- read_csv("cache/ww_data_core.csv") 
  

#############################
#Parameters:
p_val_threshold=.05 #cutoff above which plateau is defined
n_obs=5 #number of observations necessary to estimate model
baye_iter=2000 #number of iterations for bayesian model - stable at 2000
burnin = 500 # Throw away first 500 iterations - standard practive for bayesian modeling
forecast_horizon=14 #days out to forecast
trend_window=21 #number of days back to include when classifying trend (days not observations)
brks = c(0,.2,.4,.6,.8,1) #breakpoints for classification
start_obs = 20 #the number of observations used to calculate the percent rank at t 
##############################
#Define list of utilities 
utility_count <- ww_data %>%
  count(wwtp_name) %>%
  arrange(desc(n))
x=utility_count$wwtp_name[4] #pick one for testing
u_list <- utility_count$wwtp_name

for(x in u_list[1:67]){
  #subset the data to estimate BSTS
  utility_ts <- ww_data %>%
    filter(wwtp_name==x) %>%
    select(measure_date,pcr_target_avg_conc,flow_rate,population_served,pcr_target_below_lod,
           hum_frac_mic_conc,hum_frac_target_mic) %>%
    mutate(sars_frn=pcr_target_avg_conc*flow_rate/population_served,
           sars_ppn=pcr_target_avg_conc/hum_frac_mic_conc,
           sars=pcr_target_avg_conc,
           across(starts_with("sars"),log10),
           across(starts_with("sars"),~na_if(.,-Inf)),
           across(starts_with("sars"),~na_if(.,Inf))) %>%
    arrange(measure_date) 
  
  if(nrow(utility_ts)==0) next
  
  #Define max and min dates of time series
  max_date=max(utility_ts$measure_date)
  min_date=min(utility_ts$measure_date)
  
  
  bsts_dat <- map_dfr(c("sars","sars_frn","sars_ppn"),
      function(z){
        ts_var <- pull(utility_ts[,z])
        
        if(length(na.omit(ts_var))>5){
          if(exists("ss")) rm(ss)
          if(exists("fit")) rm(fit)
          ss <- AddLocalLinearTrend(list(), y=ts_var)  #I have found this best to strike balance between signal and noise
          
          fit <- bsts(
            ts_var,
            #data = utility_ts,
            state.specification = ss,
            #family = "student",
            niter = 2000
            #ping=0
          )
          
          plot_trend <- utility_ts %>%
            select(measure_date,obs=all_of(z)) %>%
            mutate(state = colMeans(fit$state.contributions[-(1:burnin),"trend",]),
                   series=z) 
        }
      }) 
  
  if(nrow(filter(bsts_dat,series=="sars"))<start_obs) next
  
  comp_dat <- bsts_dat %>%
    group_by(series) %>%
    mutate(state_pr=percentile_state(state,start_obs = start_obs,trailing = F),
           sars_bins=cut(state_pr,breaks = c(brks[1]-.01,brks[2:length(brks)]),labels = c("Very Low","Low","Moderate","High","Very High")),
           utility=x,
           state_slope=slope_fun(state,window_width=5,p_val_threshold)) %>%
    unnest(cols = c(state_slope),names_sep = "_") %>%
    mutate(state_slope_classification = factor(state_slope_classification,levels=c("Increasing","Decreasing","Plateau"),labels=c("Increasing","Decreasing","Plateau")))
  

  write_parquet(comp_dat,str_c("outputs/comp_dat/comp_dat_",make_clean_names(x),".parquet"))
  
  

  }


all_comp <- list.files("outputs/comp_dat",pattern = ".parquet",full.names = T) %>%
  map_dfr(read_parquet)

all_comp %>%
  pivot_wider(id_cols=c(utility,measure_date),
              names_from = series,
              values_from = c(state,state_pr,obs,sars_bins),
              values_fn = first) %>%
  write_csv("outputs/all_comp.csv")














#################################
#plot recent subset
recent_dat <- comp_dat %>%
  filter(measure_date %within% (as_date(today()-90) %--% today()))

bg <- group_split(recent_dat,series) %>%
  map_dfr(function(df){
    tibble(high=quantile(df$obs,probs=seq(.25, 1, 0.25),na.rm = T),
           low=quantile(df$obs,probs=c(0,seq(.25, .75, 0.25)),na.rm = T),
           clrs=str_c("#",c("2a9d8f","e9c46a","f4a261","e76f51")),
           series=df$series[1])
  })

ggplot() +
  geom_rect(data=bg,aes(xmin=as_date(today()-90),xmax=as_date(today()),
                        ymin=low,ymax=high,fill=clrs),alpha=.4,show.legend = F) +
  geom_point(data=recent_dat,aes(x=measure_date,y=obs),color="purple",alpha=.4,size=1) +
  geom_line(data=recent_dat,aes(x=measure_date,y=trend),color="darkorange") +
  labs(x=NULL,y="Normalized WW Conc. (logged)") +
  theme_bw(base_size = 14) +
  facet_wrap(~series,ncol = 1,scales = "free_y") 
        
#####################################
        


plot_trend=all_comp %>%
  filter(utility=="Fort Collins - Boxelder",
         series=="sars") %>%
  compact()

##########################################
#plot_trend=results[[1]]
#Define progress bar
pb <- progress_bar$new(total = nrow(all_comp))

#Iterate over fit and classify points
trend_out <- map_dfr(
  #compact(results),  #drops the null values
  group_split(ungroup(all_comp),utility,series),
   function(plot_trend){
     #Set date list to classify - run once with all, then 
     date_list=drop_na(plot_trend,trend) %>%
       distinct(measure_date) %>%
       pull(measure_date) #%>% 
       #last()
     
     #Construct empty list to populate
     trend_shell <- vector("list",length(date_list))
     
     for(i in 1:length(date_list)){
       pb$tick() #for progress bar
       
       #Set date to classify
       max_date=date_list[i]
       
       #Estimate linear model and classify
       trend_shell[[i]] <- plot_trend %>%
         filter(measure_date %within% (as_date(max_date-trend_window) %--% max_date)) %>%#keep only observations from past 3 weeks
         mutate(num_date=row_number()) %>% #convert date to integer - doesn't matter for single slope model but is convenient for polynomial
         lm(trend ~ num_date,data=.) %>% #fit linear model
         broom::tidy() %>% #results to dataframe
         filter(term=="num_date") %>%
         mutate(classification=case_when(
           estimate>0 & p.value<=p_val_threshold ~ "Increasing",
           estimate<0 & p.value<=p_val_threshold ~ "Decreasing",
           p.value>p_val_threshold ~ "Plateau",
         ),
         utility=na.exclude(unique(plot_trend$utility))[1],
         series=na.exclude(unique(plot_trend$series))[1],
         measure_date=max_date)
     }
     
     all_trends <- bind_rows(trend_shell) %>%
       mutate(new_class=case_when(
         classification=="Increasing" & classification==lag(classification) ~ "Steady Increasing",
         classification=="Increasing" & classification!=lag(classification) ~ "Weakly Increasing",
         classification=="Plateau" ~ "Plateau",
         classification=="Decreasing" & classification==lag(classification) ~ "Steady Decreasing",
         classification=="Decreasing" & classification!=lag(classification) ~ "Weakly Decreasing"))
     
    
     
     return(all_trends)
              
})

#merge back with ww_data 
ww_trend_final <- all_comp %>% 
  left_join(trend_out %>% select(utility,measure_date,series,classification,new_class,slope=estimate,p_val=p.value),by = c("measure_date", "utility","series"))

#cache output
write_csv(ww_trend_final,str_c("cache/utility_bsts_lm_",today(),".csv"))



#########################################
#plot heatmap of the slope
for(x in u_list){
  
  #Subset utility
  df_temp= ww_trend_final %>%
    filter(utility==x) 
  
  #Define max and min dates of time series
  max_date=max(df_temp$measure_date)
  min_date=min(df_temp$measure_date)
  
  df_temp %>%
    left_join(expand_grid(measure_date=as_date(min_date:max_date),series=unique(df_temp$series)),.) %>%
    arrange(series,measure_date) %>%
    group_by(series) %>%
    fill(obs:p_val,.direction = "down") %>%
    mutate(new_class=factor(new_class,levels=c("Steady Decreasing","Weakly Decreasing","Plateau","Weakly Increasing","Steady Increasing"))) %>%
    ggplot(aes(x=measure_date,y=series,fill=new_class)) +
    geom_tile(alpha=.6) +
    scale_fill_manual(values=str_c("#",c("2a9d8f","e9c46a","89BCBD","f4a261","e76f51"))) +
    labs(x=NULL,y=NULL,title = df_temp$utility[1]) +
    theme_bw(base_size = 14)
  
  ggsave(str_c("outputs/heatmap_trend/heat_trend_all_quartiles_",make_clean_names(x),".png"),height = 2,width = 7,units = "in")
  
}

##########################
#Do the metrics agree
upick=u_list[4]

drange <- ww_trend_final %>%
  filter(utility==upick) %>% 
  select(measure_date,obs,series) %>%
  pivot_wider(id_cols = measure_date,names_from = series,values_from = obs) %>%
  drop_na() %>%
  select(measure_date)
print(upick)

ww_trend_final %>%
  filter(utility==upick) %>% 
  inner_join(drange) %>%
  mutate(new_class=factor(new_class,levels=c("Steady Decreasing","Weakly Decreasing","Plateau","Weakly Increasing","Steady Increasing"))) %>%
  with(.,table(new_class,series)) %>%
  proportions(margin = 2) %>%  #as.data.frame()
  format(digits=1)



##########################
#Old normalization comparison plots
bg <- tibble(high=brks[2:length(brks)],
             low=brks[1:length(brks)-1],
             clrs=str_c("#",c("1495CC","2a9d8f","e9c46a","f4a261","e76f51")))


###########
# comp_dat %>%
#   ggplot(aes(x=measure_date)) +
#   geom_point(aes(y=obs),color="purple",alpha=.4,size=1) +
#   geom_line(aes(y=state),color="darkorange") +
#   scale_x_date(date_labels = "%b, %Y") +
#   labs(x=NULL,y="Normalized WW Conc. (logged)") +
#   theme_bw(base_size = 15) +
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         plot.margin = unit(c(1, 1, 1, 5),"mm")) +
#   facet_wrap(~series,ncol = 1,scales = "free_y")
# 
# ggsave("outputs/metro_normalization.png",height = 6,width = 5.5,units = "in")

ggplot() +
  geom_rect(data=bg,aes(xmin=min(comp_dat$measure_date),xmax=as_date(today()),
                        ymin=low,ymax=high,fill=clrs),alpha=.4,show.legend = F) +
  #geom_point(data=comp_dat,aes(x=measure_date,y=obs),color="purple",alpha=.4,size=1) +
  #geom_point(data=comp_dat,aes(x=measure_date,y=obs,color=sars_bins),alpha=.4,size=1) +
  geom_line(data=comp_dat,aes(x=measure_date,y=state_pr),color="darkorange") +
  #geom_line(data=comp_dat,aes(x=measure_date,y=trend,color=sars_bins,group=series)) +
  scale_fill_identity() +
  labs(x=NULL,y="Normalized WW Conc. (logged)",title = x) +
  theme_bw(base_size = 14) +
  facet_wrap(~series,ncol = 1,scales = "free_y") 

ggsave(str_c("outputs/comp_norm/comp_norm_all_quartiles_",make_clean_names(x),".png"),height = 8.7,width = 7.3,units = "in")

comp_dat %>%
  left_join(expand_grid(measure_date=as_date(min_date:max_date),series=unique(comp_dat$series)),.) %>%
  arrange(series,measure_date) %>%
  group_by(series) %>%
  fill(sars_bins,.direction = "down") %>%
  ggplot(aes(x=measure_date,y=series,fill=sars_bins)) +
  geom_tile(alpha=.6) +
  scale_fill_manual(name="Categories",values=bg$clrs) +
  labs(x=NULL,y=NULL,title = x) +
  theme_bw(base_size = 14) +
  xlim(c(as_date("2021-07-10"),as_date("2023-04-01")))

ggsave(str_c("outputs/heatmap/heatmap_all_quartiles_",make_clean_names(x),".png"),height = 2,width = 7,units = "in")

