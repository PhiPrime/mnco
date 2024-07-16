#### Save functions:

### Save.All
Save.All <- function(silent = FALSE) {
  fileName <- "Data/studentProgressHistory.csv"
  filePath <- file.path(getwd(), fileName)
  
  progress <- mutate(Update.All(get=TRUE), Date = Sys.Date())
  
  if(file.exists(filePath)) {
    dat <- read.csv(filePath)
    
    #force row numbers to be the same #Might cause errors long-term
    n <- max(c(dim(dat)[1],dim(progress)[1]))
    dim(dat)[1]      <- n
    dim(progress)[1] <- n
    
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
