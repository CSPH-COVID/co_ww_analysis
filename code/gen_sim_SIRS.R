#This script simulates wastewater data by fitting a simple SIRS model to observed hospitalization data, then applying noise scaled by the state of the system

library(pacman)
p_load(tidyverse,deSolve,janitor,lubridate,RcppRoll,conflicted)

conflict_prefer("filter", "dplyr")

source("functions/simulation_functions.R")
################################
#Read data
hosp_dat <- read_csv("input/county_hosp.csv") %>%
  mutate(measure_date=mdy(measure_date))

#Subsetting to Larimer county, linear interpolating missing values, calculate rolling average - we tried a few until getting a smoothed series
fit_dat <- hosp_dat %>%
  arrange(measure_date) %>%
  filter(county==8069) %>% #Larimer county
  distinct() %>%
  fit_approx() %>%
  drop_na(hosp) %>%
  mutate(hosp=roll_mean(roll_mean(hosp,n=21,fill=NA),n=21,fill=NA),
         week=floor(as.numeric(measure_date - min(measure_date))/7)) %>%
  drop_na(hosp) 

#Check smoothness in plot
# fit_dat %>%
#   pivot_longer(contains("hosp")) %>%
#   ggplot(aes(x=measure_date,y=value,color=name)) +
#   geom_line() +
#   facet_wrap(~name,ncol=1)

##############################
#Setting parameters
beta=.0005         # infectious contact rate (/person/day)
gamma=1/5          # recovery rate (/day)
delta=1/180       #immunity decay (/day)
init <- c(S=1000,I=12,R=2000)
sim_shell <- vector("list",max(fit_dat$week))  
misc_shell <- vector("list",length = length(sim_shell))  
week_min=min(fit_dat$week)

for(j in week_min:length(sim_shell)){ #length(sim_shell)
  print(paste0("Step ",j))
  fit_temp <- filter(fit_dat,week==j)
  dur_temp = nrow(fit_temp)
  if(dur_temp<2) next
  loss1=20000
  cnt=1
  while(loss1>10 & cnt<10){
    
    
    sim_out1 <- sim_fun(duration=dur_temp,init=init,parameters = c(beta,gamma,delta))
    loss1 <- sum((fit_temp$hosp - sim_out1$I)^2)
    
    b_up = beta*1.1
    sim_out2 <- sim_fun(duration=dur_temp,init=init,parameters = c(b_up,gamma,delta))
    loss2 <- sum((fit_temp$hosp - sim_out2$I)^2)
    
    b_down = beta*.9
    sim_out3 <- sim_fun(duration=dur_temp,init=init,parameters = c(b_down,gamma,delta))
    loss3 <- sum((fit_temp$hosp - sim_out3$I)^2)
    
    if(loss2<loss1){
      beta=b_up
    } else {
      beta=b_down
    }
    cnt=cnt+1
  }
  
  misc_shell[[j]] = tibble(beta=beta,
                           loss=loss1,
                           counter=cnt,
                           iter=j)
  sim_shell[[j]] = sim_out1[,c(2:4)]
  init=as.numeric(sim_out1[nrow(sim_out1),c(2:4)])
  names(init) <- c("S","I","R")
}

##############################
#plot fit
sim_plot <- compact(sim_shell) %>%
  bind_rows()

ggplot() +
  geom_line(data=fit_dat[1:nrow(sim_plot),],aes(x=c(1:nrow(sim_plot)),y=hosp),color="red") +
  geom_line(data=sim_plot,aes(x=c(1:nrow(sim_plot)),y=I),color="blue")

#plot loss
misc_plot <- compact(misc_shell) %>%
  bind_rows() %>%
  pivot_longer(-iter,names_to = "measure",values_to = "value")

ggplot(data=misc_plot,aes(x=iter,y=value,color=measure)) +
  geom_line() +
  facet_wrap(~measure,scales = "free")

##############################
#Now smoothing the betas and using them to resimulate with model

new_beta <- misc_plot %>%
  filter(measure=="beta") %>%
  select(week=iter,beta=value) %>%
  inner_join(select(fit_dat,measure_date,week)) 

ggplot(new_beta,aes(x=measure_date)) +
  geom_line(aes(y=beta),color="red")

sparse_dat <- new_beta %>%
  filter(weekdays(measure_date)=="Sunday") %>%
  select(measure_date,beta)

interp_beta <- approx(sparse_dat$measure_date,sparse_dat$beta,new_beta$measure_date) %>%
  as_tibble() %>%
  rename(measure_date=x,int_beta=y)

new_beta <- new_beta %>%
  inner_join(interp_beta)

ggplot(new_beta,aes(x=measure_date)) +
  geom_line(aes(y=beta),color="red") +
  geom_line(aes(y=int_beta),color="purple")

#simulate disease
params <- new_beta %>%
  select(beta=int_beta) %>%
  add_column(gamma=1/5,
             delta=1/180)

smooth_sim <- sim_fun(duration = nrow(new_beta),init = c(S=1000,I=20,R=2000),parameters = params)

smooth_sim %>%
  add_column(inner_join(interp_beta,fit_dat) %>% select(hosp)) %>%
  ggplot(.,aes(x=time)) +
  geom_line(aes(y=I),color="red") +
  geom_line(aes(y=hosp),color="blue")
