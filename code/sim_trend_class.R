library(pacman)
p_load(tidyverse,lubridate,RcppRoll,conflicted,bsts)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

source("functions/simulation_functions.R")
source("functions/analysis_functions.R")

sim_dat <- read_csv("cache/simulated_data.csv") %>%
  dplyr::mutate(conc=log10(conc),
                rolling=log10(rolling),
                sim_pred=log10(sim_pred))

#############################
#Parameters:
p_val_threshold=.05 #cutoff above which plateau is defined
n_obs=5 #number of observations necessary to estimate model
baye_iter=2000 #number of iterations for bayesian model - stable at 2000
burnin = 500 # Throw away first 500 iterations - standard practive for bayesian modeling
forecast_horizon=10 #days out to forecast
trend_window=21 #number of days back to include when classifying trend (days not observations)

##############################
rm(list = c("ss","fit","pred_ww"))
ss <- AddLocalLinearTrend(list(), y=sim_dat$sim_pred)  #I have found this best to strike balance between signal and noise

fit <- bsts(
  sim_dat$sim_pred,
  state.specification = ss,
  #family = "student",
  niter = 2000 #, ping=0
)

#Plot fitted line
#plot(fit,"state")

#Forecast x days with model
pred_ww <- predict(fit, horizon = forecast_horizon,burn = burnin,quantiles = c(.05,.95))

#Build dataframe from model fit results
plot_trend <- sim_dat %>%
  mutate(est_trend = colMeans(fit$state.contributions[-(1:burnin),"trend",]))

plot_trend %>%
  dplyr::select(sample_collect_date,Simulated=rolling,Estimated=est_trend) %>%
  pivot_longer(-sample_collect_date) %>%
  ggplot(aes(x=sample_collect_date,y=value,color=name)) +
  geom_line() +
  scale_x_date(date_labels = "%Y %b") +
  #scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  scale_color_discrete(name=NULL) +
  labs(x=NULL,y="Wastewater Concentrations (log)") +
  theme_bw(base_size = 12) +
  theme(legend.position = c(.28,.8),
        legend.background = element_blank())

ggsave("outputs/ww_sim_compare.png",width = 4.5,height = 4,units = "in")


#calculate the slope at all points
slope_dat <- plot_trend %>%
  mutate(across(c(conc,est_trend,sim_pred),~slope_fun(.,window_width=5,p_val_threshold))) %>%
  select(sample_collect_date,conc,est_trend,sim_pred) %>%
  unnest(cols = c(conc, est_trend, sim_pred),names_sep = "_") 
  
slope_dat %>%
  select(sample_collect_date,contains("slope")) %>%
  pivot_longer(-sample_collect_date) %>%
  ggplot(aes(x=sample_collect_date,y=value,color=name)) +
  geom_point() +  
  scale_color_discrete(name=NULL) +
  labs(x=NULL,y="Series Slope") +
  theme_bw(base_size = 14) +
  theme(legend.position = c(.2,.9),
        legend.background = element_blank())

ggsave("outputs/ww_sim_slope_compare.png",width = 4.5,height = 4,units = "in")


slope_dat %>%
  ggplot(aes(x=est_hosp_slope,y=hosp_ma_slope)) +
  geom_point()

###########
#can we generate a table like a confusion matrix of some sort
#rolling is truth and est_trend is modeled
library(yardstick)

cm <- plot_trend %>%
  mutate(across(c(est_trend,rolling),~slope_fun(.,window_width=5,p_val_threshold))) %>%
  select(sample_collect_date,est_trend,rolling) %>%
  unnest(cols = c(est_trend, rolling),names_sep = "_") %>%
  mutate(across(contains("classification"),~factor(.,levels=c("Increasing","Decreasing","Plateau"),labels=c("Increasing","Decreasing","Plateau")))) %>%
  conf_mat(truth = rolling_classification, estimate = est_trend_classification)

cm
summary(cm)

library(xtable)
xtable(cm$table,
       caption = c("Confusion matrix comparing the trend in the true but unobserved state (Truth) to the trend estimate based on the BSTS model (rows)"),
       label = "tab:confusion_matrix",
       digits = 0,
       auto = TRUE) 


#compare to not using BSTS to estimate trend



#############
#Can we find the lag window that maximizes accuracy
acc_mat <- map_dfr(c(3:20),
                   function(x){
                     slope_temp <- plot_trend %>%
                       mutate(across(c(est_trend,rolling),~slope_fun(.,window_width=x,p_val_threshold))) %>%
                       select(sample_collect_date,est_trend,rolling) %>%
                       unnest(cols = c(est_trend, rolling),names_sep = "_") %>%
                       mutate(across(contains("classification"),~factor(.,levels=c("Increasing","Decreasing","Plateau"),labels=c("Increasing","Decreasing","Plateau"))))
                     
                     cm <- summary(conf_mat(slope_temp,truth = rolling_classification, estimate = est_trend_classification))[1,3] %>%
                       add_column(lag=x)
                     
                     return(cm)
                   })

ggplot(acc_mat,aes(x=lag,y=.estimate)) +
  geom_point()
