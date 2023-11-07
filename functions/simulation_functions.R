#Simulation functions

fit_approx <- function(dat){
  #Helper function to approximate fit data
  full_date_range <- seq.Date(min(dat$measure_date),max(dat$measure_date),by=1)
  
  out <- approx(dat$measure_date,dat$hosp,full_date_range) %>%
    as_tibble() %>%
    rename(measure_date=x,hosp=y)
  
  return(out)
}


#################################
##Define the SIR
eqn <- function(time,state,parameters){
  with(as.list(c(state,parameters)),{
    dS <- -beta*S*I + delta*R    #Susceptible
    dI <- beta*S*I - gamma*I       #Infected
    dR <- gamma*I - delta*R                     #Removed
    return(list(c(dS,dI,dR)))}
  )
}

#############################
#Fitting algorithm - rough approximation
#The approach is to minimize the loss function over 1 week window

sim_fun <- function(duration,init,parameters){
  time=seq(0,duration-1,by=1)
  shell <- vector("list",length(time))  #I made this dynamic
  shell[[1]] <- init
  for(i in c(1:duration)){
    if(is.null(nrow(parameters))){
      param.temp <- parameters
    } else {
      param.temp <- parameters[i,]
    }
    names(param.temp) <- c("beta","gamma","delta")
    
    shell[[i]] <- ode(y=init,times=c(time[i],time[i]+1),eqn,parms=param.temp,method="ode45")
    
    init = shell[[i]][2,2:4]
    
  }
  f.out <- map_dfr(shell,~.[2,])
  return(f.out)
}