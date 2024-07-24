#### Save functions:

### Save.All
Save.All <- function(silent = FALSE) {

}

### Save.History
# Appends the output of Update.All() to centerHistory.rds
Save.History <- function(date = Sys.Date(), ignoreMissing = F, silent = F) {
  fileName <- "Cache/centerHistory.rds"
  filePath <- file.path(getwd(), fileName)
  
  history <- mutate(Update.All(get=TRUE), Date_Saved = Sys.Date(), .before = "Account_Id")
  
  # Remove [ and ] from column names
  names(history) <- gsub("[][]", "", names(history))
  
  if(file.exists(filePath)) {
    dat <- readRDS(filePath)
    dat <- rbind(dat[dat$Date != Sys.Date(),], history)
  } else {
    dat <- history
  }
  
  saveRDS(dat, filePath)
  
  if(!silent) print("Student data saved!")
}
