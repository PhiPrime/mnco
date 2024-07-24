#### Save functions:

### Save.All
Save.All <- function(silent = FALSE) {

}

### Save.History
# Appends the output of Update.All() to centerHistory.rds
# WORKS ONLY FOR CURRENT DATE - NEED TO PASS DATE ARGUMENT INTO UPDATE.ALL()
# MAYBE CHECK IF STRUCTURE DIFFERENT THEN CALL SAVE.ALL()
Save.History <- function(date = Sys.Date(), ignoreMissing = F, silent = F) {
  fileName <- "Cache/centerHistory.rds"
  filePath <- file.path(getwd(), fileName)
  
  history <- mutate(Update.All(get=TRUE), Date = date, .before = "Account_Id")
  
  # Remove [ and ] from column names
  # PUT THIS IN AN UPDATE FUNCTION
  names(history) <- gsub("[][]", "", names(history))
  
  if(file.exists(filePath)) {
    dat <- readRDS(filePath)
    dat <- rbind(dat[dat$Date != Sys.Date(),], history)
  } else {
    dat <- history
  }
  
  saveRDS(dat, filePath)
  
  if(!silent) cat("Notice: Center history saved for ", as.character(date), "!", sep="")
}
