#### Save functions:

### Save.All
Save.All <- function(silent = FALSE) {
  fileName <- "Student_Progress_History.csv"
  filePath <- file.path(dataDir, fileName)
  
  progress <- mutate(Update.All(get=TRUE), Date = Sys.Date())
  
  if(file.exists(filePath)) {
    dat <- read.csv(filePath)
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
