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

#Percentile the state and classify
brks <- c(0,.2,.4,.6,.8,1)

perc_state <- plot_trend %>%
  mutate(across(c(rolling,est_trend),~percentile_state(.,start_obs = 40,trailing = F),.names = "{.col}_ps_all"),
         across(c(rolling,est_trend),~percentile_state(.,start_obs = 40,trailing = T,trail_length = 25),.names = "{.col}_ps_trailing"),
         across(contains("ps"),~cut(.,breaks = c(brks[1]-.01,brks[2:length(brks)]),labels = c("Very Low","Low","Moderate","High","Very High")),.names = "{.col}_class"))


bg <- tibble(high=brks[2:length(brks)],
             low=brks[1:length(brks)-1],
             clrs=str_c("#",c("1495CC","2a9d8f","e9c46a","f4a261","e76f51")))

ps_plot <- perc_state %>%
  select(sample_collect_date,Simulated=rolling_ps_all,Estimated=est_trend_ps_all) %>%
  pivot_longer(-sample_collect_date) 

ggplot() +
  geom_rect(data=bg,aes(xmin=as_date(min(perc_state$sample_collect_date)),xmax=as_date(max(perc_state$sample_collect_date)),
                        ymin=low,ymax=high,fill=clrs),alpha=.4,show.legend = F) +
  geom_line(data=ps_plot,aes(x=sample_collect_date,y=value,linetype=name)) +
  scale_x_date(date_labels = "%Y %b") +
  scale_y_continuous(breaks = brks,labels = c("0%","20%","40%","60%","80%","100%")) +
  scale_fill_identity() +
  scale_linetype(name=NULL) +
  labs(x=NULL,y="Wastewater Concentrations (percent rank)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(.70,.2),
        legend.background = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA))

ggsave("outputs/ww_sim_state_class_compare.png",width = 4.5,height = 4,units = "in")


library(yardstick) #package to calculate confusion matrix
library(xtable) #package to format confusion matrix table

#Confusion matrix for state classification based on All data
cm_state_all <- perc_state %>%
  select(sample_collect_date,est_trend_ps_all_class,rolling_ps_all_class) %>%
  conf_mat(truth = rolling_ps_all_class, estimate = est_trend_ps_all_class)

cm_state_all
summary(cm_state_all)


print(xtable(cm_state_all$table,
       caption = c("Confusion matrix comparing the true but unobserved state (columns) to the BSTS state estimate (rows). "),
       label = "tab:confusion_matrix_state_all",
       digits = 0,
       auto = TRUE),
      add.to.row = list(pos=list(5),command="\\hline \\\\\n \\multicolumn{6}{l}{\\footnotesize{Note that states are classified based on the following ranges: Very Low (0-20\\%], Low (20-40\\%], Moderate (40-60\\%], High (60-80\\%], Very High (80-100\\%]} }"),
      hline.after = c(-1,0)
)

#Confusion matrix for state classification based on trailing 90 data
cm_state_trailing <- perc_state %>%
  select(sample_collect_date,est_trend_ps_trailing_class,rolling_ps_trailing_class) %>%
  conf_mat(truth = rolling_ps_trailing_class, estimate = est_trend_ps_trailing_class)

cm_state_trailing
summary(cm_state_trailing)


print(xtable(cm_state_trailing$table,
             caption = c("Confusion matrix comparing the true but unobserved state (columns) to the BSTS state estimate (rows). "),
             label = "tab:confusion_matrix_state_trailing",
             digits = 0,
             auto = TRUE),
      add.to.row = list(pos=list(5),command="\\hline \\\\\n \\multicolumn{6}{l}{\\footnotesize{Note that states are classified based on the following ranges: Very Low (0-20\\%], Low (20-40\\%], Moderate (40-60\\%], High (60-80\\%], Very High (80-100\\%]} }"),
      hline.after = c(-1,0)
)

###########
#can we generate a table like a confusion matrix of some sort
#rolling is truth and est_trend is modeled


cm <- plot_trend %>%
  mutate(across(c(est_trend,rolling),~slope_fun(.,window_width=5,p_val_threshold))) %>%
  select(sample_collect_date,est_trend,rolling) %>%
  unnest(cols = c(est_trend, rolling),names_sep = "_") %>%
  mutate(across(contains("classification"),~factor(.,levels=c("Increasing","Decreasing","Plateau"),labels=c("Increasing","Decreasing","Plateau")))) %>%
  conf_mat(truth = rolling_classification, estimate = est_trend_classification)

cm
summary(cm)


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
