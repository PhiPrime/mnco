#### Save functions:

### Save.All
Save.All <- function(silent = FALSE) {
  fileName <- "Cache/centerHistory.rds"
  filePath <- file.path(getwd(), fileName)
  
  progress <- mutate(Update.All(get=TRUE), Date = Sys.Date())
  #Need some gsubs to change [ or ] to .
  names(progress) <- gsub("[][ ]", ".", names(progress))
  
  if(file.exists(filePath)) {
    dat <- readRDS(filePath)
    dat <- rbind(dat[dat$Date != Sys.Date(),], progress)
  } else {
    dat <- progress
  }
  
  saveRDS(dat, filePath)
  
  if(!silent) print("Student data saved!")
}

### Save.Backlog
Save.Backlog <- function(silent = FALSE) {
  
}
