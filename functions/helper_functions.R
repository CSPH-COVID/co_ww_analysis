#Helper functions

#This function builds out the folder structure
folder_setup <- function(){
  folder.list <- c("code",
                   "cache",
                   "input",
                   "outputs")
  
  lapply(folder.list,
      function(x){
        if(!dir.exists(x)){
          dir.create(x,recursive = T)
          message(paste0("The ",x," folder has been created."))
        } else {
          message(paste0("The ",x," folder already exists."))
        }
      })
  
  return(NULL)
}


