#### Save functions:

### Save.All
Save.All <- function(silent = FALSE) {
  fileName <- "Cache/centerHistory.rds"
  filePath <- file.path(getwd(), fileName)
  
  history <- mutate(Update.All(get=TRUE), Date = Sys.Date())
  #Need some gsubs to change [ or ] to .
  names(history) <- gsub("[][ ]", ".", names(history))
  
  if(file.exists(filePath)) {
    dat <- readRDS(filePath)
    dat <- rbind(dat[dat$Date != Sys.Date(),], history)
  } else {
    dat <- history
  }
  
  saveRDS(dat, filePath)
  
  if(!silent) print("Student data saved!")
}

### Save.Backlog
Save.Backlog <- function(silent = FALSE) {
  
}
