#### Save functions:

### saveAllCenterData
# FIGURE OUT IF SHOULD SAVE DATES WITH MISSING FILES
saveAllCenterData <- function(startDate = mdy("1/1/2020"), endDate = Sys.Date()) {
  fileName <- "Cache/centerHistory.rds"
  filePath <- file.path(getwd(), fileName)
  date <- startDate
  
  # Delete centerHistory.rds
  # ADD PROMPT?
  cat("CAUTION: \"", filePath, "\" will now be deleted!\n", sep = "")
  if (file.exists(filePath)) file.remove(filePath)
  
  saveCount <- 0
  failCount <- 0
  
  # VERY HACKY, PLEASE HANDLE PROPERLY LATER
  while (date <= endDate) {
    # TEST ignoreMissing = (date != Sys.Date())
    
    failed <- tryCatch(
      saveCenterData(date, silent = T),
      error = function(e) {
        return(TRUE)
      }
    )
    failed <- ifelse(is.null(failed), FALSE, failed)
    
    if (failed) failCount <- failCount + 1
    if (!failed) saveCount <- saveCount + 1
    date <- date + days(1)
  }
  
  # PRINT SUCCESS MESSAGE
  cat("SUCCESS: Center history saved for ", saveCount, " dates", sep="")
  cat(", failed", failCount, "times\n")
}

### saveCenterData
# Appends the output of getCenterData() to centerHistory.rds
# MAYBE CHECK IF STRUCTURE DIFFERENT THEN CALL saveAllCenterData() - USE ALL.EQUAL?
saveCenterData <- function(date = Sys.Date(), ignoreMissing = F, silent = F) {
  fileName <- "Cache/centerHistory.rds"
  filePath <- file.path(getwd(), fileName)
  
  history <- mutate(getCenterData(date, ignoreMissing), Date = date, 
                    .before = "Account_Id")
  
  # Remove [ and ] from column names
  # PUT THIS IN AN UPDATE FUNCTION
  names(history) <- gsub("[][]", "", names(history))
  
  if(file.exists(filePath)) {
    dat <- readRDS(filePath)
    dat <- rbind(dat[dat$Date != Sys.Date(),], history)
  } else {
    # ADD SILENT CHECK HERE?
    cat("NOTICE: ", filePath, " does not exist.", 
        "\n\tCreating ", fileName, "...\n", sep="")
    dat <- history
  }
  
  saveRDS(dat, filePath)
  
  if(!silent) cat("SUCCESS: Center history saved for ", as.character(date), 
                  "!\n", sep="")
}
