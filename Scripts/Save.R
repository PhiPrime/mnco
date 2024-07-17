#### Save functions:

### Save.All
Save.All <- function(silent = FALSE) {
  fileName <- "Cache/studentProgressHistory.csv"
  filePath <- file.path(getwd(), fileName)
  
  progress <- mutate(Update.All(get=TRUE), Date = Sys.Date())
  #Need some gsubs to change [ or ] to .
  
  if(file.exists(filePath)) {
    dat <- read.csv(filePath)
    
    #Check if names(dat)!=names(progress)
    # then set names to equal eachother, colacse dat to
    #  have default values for new vars
    
    #force col numbers to be the same #Might cause errors long-term
    n <- max(c(dim(dat)[2],dim(progress)[2]))
    dim(dat)[2]      <- n
    dim(progress)[2] <- n
    
    dat <- rbind(dat[dat$Date != Sys.Date(),], progress)
  } else {
    dat <- progress
  }
  
  write.csv(dat, fileName, row.names = FALSE)
  
  if(!silent) print("Student data saved!")
}

### Save.Backlog
Save.Backlog <- function(silent = FALSE) {
  
}
