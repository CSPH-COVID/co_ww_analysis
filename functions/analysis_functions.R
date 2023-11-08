#Analysis functions

slope_fun <- function(x,window_width=4){
  wind_param <- floor(window_width/2)
  slope_est=vector(length = length(x))
  for(i in wind_param:(length(x)-wind_param)){
    dsub <- x[(i-wind_param):(i+wind_param)]
    slope_est[i] <- coef(lm(dsub ~ c(1:length(dsub))))[2]
  }
  return(slope_est)
}