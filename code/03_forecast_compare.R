library(pacman)
p_load(tidyverse,lubridate,conflicted,bsts)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")

#############################
ww_data <- read_csv("cache/ww_data_core.csv")

#############################
#Parameters:
p_val_threshold=.05 #cutoff above which plateau is defined
n_obs=5 #number of observations necessary to estimate model
baye_iter=2000 #number of iterations for bayesian model - stable at 2000
burnin = 500 # Throw away first 500 iterations - standard practive for bayesian modeling
forecast_horizon=14 #days out to forecast

##############################

#Define list of utilities 
utility_count <- ww_data %>%
  count(wwtp_name) %>%
  arrange(desc(n))
x=utility_count$wwtp_name[14] #pick one for testing

utility_ts <- ww_data %>%
  filter(wwtp_name==x) %>%
  select(measure_date,sars=pcr_target_avg_conc) %>%
  mutate(across(starts_with("sars"),log10),
         across(starts_with("sars"),~na_if(.,-Inf)),
         across(starts_with("sars"),~na_if(.,Inf))) %>%
  arrange(measure_date) 

utility_ts_subset <- utility_ts %>%
  filter(measure_date < as_date("2022-04-26"))

#Define max and min dates of time series
max_date=max(utility_ts_subset$measure_date)
min_date=min(utility_ts_subset$measure_date)

ts_var <- pull(utility_ts_subset[,"sars"])

#BSTS
ss <- AddLocalLinearTrend(list(), y=ts_var)  

fit <- bsts(
  ts_var,
  #data = utility_ts,
  state.specification = ss,
  #family = "student",
  niter = baye_iter
  #ping=0
)


pred_error <- bsts.prediction.errors(fit,cutpoints = c(1,2,3,4),burn = burnin)
pred_error_sum <- pred_error[[1]] %>%
  apply(.,2,mean)
plot(cumsum(abs(pred_error_sum)))
plot(pred_error_sum)
hist(pred_error_sum)
PlotBstsPredictionErrors(fit,cutpoints = c(1,2,3,4),burn = burnin)

pred_ww <- predict(fit, horizon = forecast_horizon,burn = burnin,quantiles = c(.05,.25,.75,.95))

plot_trend <- utility_ts_subset %>%
  select(measure_date,obs=sars) %>%
  mutate(state = colMeans(fit$state.contributions[-(1:burnin),"trend",])) %>%
  bind_rows(
    tibble(measure_date = as_date(max_date + c(1:length(pred_ww$mean))),
           state=pred_ww$mean,
           lower_5_ci=pred_ww$interval[1,],
           lower_25_ci=pred_ww$interval[2,],
           upper_75_ci=pred_ww$interval[3,],
           upper_95_ci=pred_ww$interval[4,])
  ) %>%
  mutate(utility=x)


plot_trend %>%
  ggplot(aes(x=measure_date)) +
  geom_point(aes(y=obs)) +
  geom_point(data=utility_ts %>% filter(measure_date>max_date),aes(y=sars),color="orange",shape="triangle") +
  geom_line(aes(y=state),color="purple") +
  geom_ribbon(aes(ymin=lower_5_ci,ymax=upper_95_ci),alpha=.2,fill="darkorange") +
  geom_ribbon(aes(ymin=lower_25_ci,ymax=upper_75_ci),alpha=.3,fill="darkorange") +
  xlim(as_date("2022-01-01"),as_date("2022-06-01")) +
  theme_bw(base_size = 12) +
  labs(x="2022",y="WW Concentration (logged)")

ggsave("outputs/forecast.png",height=4.5,width=5,units = "in")
#ggsave("outputs/forecast.eps",height=4.5,width=5,units = "in")
ggsave("outputs/forecast.pdf",height=4.5,width=5,units = "in")
