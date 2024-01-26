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



