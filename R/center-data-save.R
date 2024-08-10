##########################     SAVE FUNCTIONS     ###########################

# Appends the output of getCenterData() to centerHistory.rds
# MAYBE CHECK IF STRUCTURE DIFFERENT THEN CALL saveAllCenterData() - USE ALL.EQUAL?
saveCenterData <- function(date = Sys.Date(), ignoreMissing = F, silent = F) {
  filePath <- file.path(cacheDir(), "centerHistory.rds")

  history <- dplyr::mutate(getCenterData("all", date, ignoreMissing), Date = date,
                           .before = "Account_Id")

  # Remove [ and ] from column names
  # PUT THIS IN AN UPDATE FUNCTION
  names(history) <- gsub("[][]", "", names(history))

  if(file.exists(filePath)) {
    dat <- readRDS(filePath)
    dat <- rbind(dat[dat$Date != Sys.Date(),], history)
  } else {
    # ADD SILENT CHECK HERE?
    # -We should start using message() and warning() in addition to stop()
    message("NOTICE: ", filePath, " does not exist.",
                "\n\tCreating ", filePath, "...\n", sep="")
    dat <- history
  }

  saveRDS(dat, filePath)

  if(!silent) message("SUCCESS: Center history saved for ", as.character(date),
                  "!\n", sep="")
}

# FIGURE OUT IF SHOULD SAVE DATES WITH MISSING FILES
saveAllCenterData <- function(startDate = as.Date("2020-01-01"), endDate = Sys.Date(),
                              promptDelete = T) {
  filePath <- file.path(cacheDir(), "centerHistory.rds")
  date <- startDate

  # Delete centerHistory.rds
  if (file.exists(filePath)) {
    message("CAUTION: \"", filePath,
                "\" will now be deleted and recreated!\n", sep = "")

    if (promptDelete) {
      input <- readline(prompt = "\tAre you sure you want to proceed? (y/n): ")

      if (tolower(input) != "y") {
        message("NOTICE: Save aborted!")
        return(invisible())
      }
    }
    file.remove(filePath)
  }

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
    date <- date + 1
  }

  # PRINT SUCCESS MESSAGE
  message("SUCCESS: Center data saved for ", saveCount,
              " dates, ", sep="")

  message(failCount, " dates skipped (incomplete set)\n", sep="")
}
