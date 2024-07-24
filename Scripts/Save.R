#### Save functions:

### Save.All
# NEED TO PASS IGNORE MISSING ARGUMENT INTO UPDATE FUNCTIONS
Save.All <- function(startDate = mdy("1/1/2020"), endDate = Sys.Date()) {
  date <- startDate
  
  while (date <= endDate) {
    # TEST THIS SAVE
    Save.History(date, ignoreMissing = date != Sys.Date(), 
                 silent = T)
    date <- date + days(1)
  }
  
  # PRINT SUCCESS MESSAGE
}

### Save.History
# Appends the output of Update.All() to centerHistory.rds
# MAYBE CHECK IF STRUCTURE DIFFERENT THEN CALL SAVE.ALL() - USE ALL.EQUAL?
Save.History <- function(date = Sys.Date(), ignoreMissing = F, silent = F) {
  fileName <- "Cache/centerHistory.rds"
  filePath <- file.path(getwd(), fileName)
  
  history <- mutate(Update.All(TRUE, date), Date = date, .before = "Account_Id")
  
  # Remove [ and ] from column names
  # PUT THIS IN AN UPDATE FUNCTION
  names(history) <- gsub("[][]", "", names(history))
  
  if(file.exists(filePath)) {
    dat <- readRDS(filePath)
    dat <- rbind(dat[dat$Date != Sys.Date(),], history)
  } else {
    cat("Notice: ", filePath, " does not exist.", 
        "\n\tCreating ", fileName, "...", sep="")
    dat <- history
  }
  
  saveRDS(dat, filePath)
  
  if(!silent) cat("Success: Center history saved for ", as.character(date), "!", sep="")
}
