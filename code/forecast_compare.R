
ww_data_1 <- read_csv("input/shared/1_SARS-CoV-2_Wastewater_Data_ 2023-04-24 .csv")

ww_data <- ww_data_1 %>%
  select(wwtp_name,sample_collect_date,pcr_target_avg_conc,pcr_target_std_error,quality_flag,flow_rate,population_served,
         pcr_target_below_lod,hum_frac_mic_conc,hum_frac_target_mic) %>%
  mutate(sample_collect_date=mdy(sample_collect_date))

#############################
#Parameters:
p_val_threshold=.05 #cutoff above which plateau is defined
n_obs=5 #number of observations necessary to estimate model
baye_iter=2000 #number of iterations for bayesian model - stable at 2000
burnin = 500 # Throw away first 500 iterations - standard practive for bayesian modeling
forecast_horizon=14 #days out to forecast
trend_window=21 #number of days back to include when classifying trend (days not observations)

##############################

#Define list of utilities 
utility_count <- ww_data %>%
  count(wwtp_name) %>%
  arrange(desc(n))
x=utility_count$wwtp_name[14] #pick one for testing

utility_ts <- ww_data %>%
  filter(wwtp_name==x) %>%
  select(sample_collect_date,pcr_target_avg_conc,flow_rate,population_served,pcr_target_below_lod,
         hum_frac_mic_conc,hum_frac_target_mic) %>%
  mutate(measure_date=as_date(sample_collect_date),
         sars_frn=pcr_target_avg_conc*flow_rate/population_served,
         sars_ppn=pcr_target_avg_conc/hum_frac_mic_conc,
         sars=pcr_target_avg_conc,
         across(starts_with("sars"),log10),
         across(starts_with("sars"),~na_if(.,-Inf)),
         across(starts_with("sars"),~na_if(.,Inf))) %>%
  arrange(measure_date) 

ggplot(utility_ts,aes(x=measure_date,y=sars)) +
  geom_line()

utility_ts_subset <- utility_ts %>%
  filter(measure_date < as_date("2022-04-26"))

#Define max and min dates of time series
max_date=max(utility_ts_subset$measure_date)
min_date=min(utility_ts_subset$measure_date)


x="sars"
ts_var <- pull(utility_ts_subset[,x])


ss <- AddLocalLinearTrend(list(), y=ts_var)  #I have found this best to strike balance between signal and noise - it let's the state drift slowly

fit <- bsts(
  ts_var,
  #data = utility_ts,
  state.specification = ss,
  #family = "student",
  niter = 2000
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
  select(measure_date,obs=all_of(x)) %>%
  mutate(trend = colMeans(fit$state.contributions[-(1:burnin),"trend",]),
         series=x) %>%
  bind_rows(
    tibble(measure_date = as_date(max_date + c(1:length(pred_ww$mean))),
           trend=pred_ww$mean,
           sars=NA,
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
  geom_line(aes(y=trend),color="purple") +
  geom_ribbon(aes(ymin=lower_5_ci,ymax=upper_95_ci),alpha=.2,fill="darkorange") +
  geom_ribbon(aes(ymin=lower_25_ci,ymax=upper_75_ci),alpha=.3,fill="darkorange") +
  xlim(as_date("2022-01-01"),as_date("2022-06-01")) +
  theme_bw(base_size = 12) +
  labs(x="2022",y="WW Concentration (logged)")

ggsave("outputs/forecast.png",height=4.5,width=5,units = "in")
ggsave("outputs/forecast.eps",height=4.5,width=5,units = "in")
ggsave("outputs/forecast.pdf",height=4.5,width=5,units = "in")
