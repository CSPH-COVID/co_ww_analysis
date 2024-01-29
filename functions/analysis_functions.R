#Analysis functions

slope_fun <- function(x,window_width=4,p_val_threshold){
  #estimate slope based on trailing data of length window_width
  #wind_param <- floor(window_width/2)
  reg_out=vector("list",length = length(x))
  for(i in 1:length(x)){
    if(i<window_width){ 
      reg_out[[i]] <- c(`Estimate`=NA,`Pr(>|t|)`=NA)
      next
      }
    dsub <- x[(i-wind_param):(i+wind_param)]
    reg_out[[i]] <- summary(lm(dsub ~ c(1:length(dsub))))$coefficients[2,c(1,4)] #extract estimate and pval
    #slope_est[i] <- coef(reg_out)[2]
    #slope_pval[i] <- summary(reg_out)$coefficients
  }
  reg_out <- bind_rows(reg_out) 
  names(reg_out) <- c("slope","p_val")
  reg_out <- mutate(reg_out,
  classification=case_when(
    slope>0 & p_val<=p_val_threshold ~ "Increasing",
    slope<0 & p_val<=p_val_threshold ~ "Decreasing",
    p_val>p_val_threshold ~ "Plateau",
  ))
  return(reg_out)
}
#Test
#check_sim <- slope_fun(plot_trend$rolling,6,p_val_threshold)



#####################
#Function to calculate the percentile of an observation given the history up to time t
#x=plot_trend$rolling
#start_obs = 30
percentile_state <- function(x,start_obs,trailing=F,trail_length=25){
  if(start_obs<10) warning("start_obs is less than 10 observations. There are not many observations to calculate the percentile.")
  
  drange=c(start_obs:length(x))
  shell <- rep(NA,length(x))
  
  if(trailing){
    if(trail_length>start_obs){
      warning("Trail length is set to a number larger than the start observation. Increasing the start observation.")
      start_obs = trail_length+1
      drange=c(start_obs:length(x))
    }
    for(i in drange){
      up_to_t = x[(i-trail_length):(i-1)]
      shell[i] <- sum(up_to_t <= x[i]) / length(up_to_t)
    }
  } else {
    #for each observation, calculate the percentile on the set x:t-1
    for(i in drange){
      up_to_t = x[1:(i-1)]
      shell[i] <- sum(up_to_t <= x[i]) / length(up_to_t)
    }
  }
  return(shell)
  
}


  
