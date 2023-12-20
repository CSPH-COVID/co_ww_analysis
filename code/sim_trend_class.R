library(pacman)
p_load(tidyverse,lubridate,RcppRoll,conflicted,bsts)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

source("functions/simulation_functions.R")
source("functions/analysis_functions.R")

sim_dat <- read_csv("cache/simulated_data.csv") %>%
  dplyr::mutate(rolling=log10(rolling),
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

ss <- AddLocalLinearTrend(list(), y=sim_dat$sim_pred)  #I have found this best to strike balance between signal and noise

fit <- bsts(
  sim_dat$sim_pred,
  state.specification = ss,
  family = "student",
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
  mutate(across(c(hosp_ma,est_hosp,sim_pred),~slope_fun(.,window_width=4),.names = "{.col}_slope")) %>%
  select(measure_date,contains("slope"))
  
slope_dat %>%
  pivot_longer(-measure_date) %>%
  ggplot(aes(x=measure_date,y=value,color=name)) +
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

#compare to not using BSTS to estimate trend


#############3
#Test if we simply used the moving average
