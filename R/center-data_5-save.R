# MAYBE CHECK IF STRUCTURE DIFFERENT THEN CALL saveAllCenterData() - USE ALL.EQUAL?
#' Store center data in .csv file
#'
#' @param date Date to save data for
#' @param ignoreMissing `logical` indicating if data should be saved if there
#'  is missing data
#' @param silent `logical`. Determines if messages should be printed.
#'
#' @return None (invisible `NULL`)
#' @export
#'
#' @examples
#' saveCenterData()
saveCenterData <- function(date = Sys.Date(), silent = F) {
  data <- getCenterData("all", date) %>%
    mutate(Date_Saved = date, .before = "Account_Id")

  filePath <- file.path(cacheDir(), "centerHistory.rds")

  if (!file.exists(filePath)) {
    if (!silent) message("NOTICE: ", filePath, " does not exist.",
                         "\n\tCreating ", filePath, "...\n", sep="")
    history <- data
  } else {
    history <- readRDS(filePath) %>%
      dplyr::rows_upsert(data, by = c("Student", "Date_Saved"))
  }

  saveRDS(history, filePath)

  if(!silent) message("SUCCESS: Center history saved for ", as.character(date),
                  "!\n", sep="")

  invisible(NULL)
}

# FIGURE OUT IF SHOULD SAVE DATES WITH MISSING FILES
#' Save center data for range of dates
#'
#' @param startDate A date. The default is `"2020-01-01"`.
#' @param endDate A date. The default is the current date.
#' @param promptDelete `logical` indicating if the user should confirm the
#'  recreation of the file
#'
#' @return None (invisible `NULL`)
#' @export
#'
#' @examples
#' saveAllCenterData()
saveAllCenterData <- function(startDate = as.Date("2020-01-01"),
                              endDate = Sys.Date(),
                              silent = F) {
  filePath <- file.path(cacheDir(), "centerHistory.rds")

  # Delete centerHistory.rds
  if (file.exists(filePath)) {
    if (!silent) {
      message("CAUTION: \"", filePath,
                  "\" will now be deleted and recreated!", sep = "")
      input <- readline(prompt = "Are you sure you want to proceed? (y/n): ")

      if (tolower(input) != "y") {
        message("NOTICE: Save aborted!")
        return(invisible())
      }
    }
    file.remove(filePath)
  }

  saveCount <- 0
  failCount <- 0

  # VERY HACKY, PLEASE HANDLE PROPERLY LATER (MAYBE)
  fileDates <- list.files(rawDataDir()) %>%
    stringr::str_extract_all("\\d{1,2}_\\d{1,2}_\\d{4}") %>%
    unlist() %>%
    unique() %>%
    as.Date(format = "%m_%d_%Y") %>%
    sort()

  for (date in as.list(fileDates)) {
    # TEST ignoreMissing = (date != Sys.Date())
    if (date < startDate) next
    if (date > endDate) next

    failed <- tryCatch(
      saveCenterData(date, silent = T),
      error = function(e) {
        return(TRUE)
      }
    )
    failed <- ifelse(is.null(failed), FALSE, failed)

    if (failed) failCount <- failCount + 1
    if (!failed) saveCount <- saveCount + 1
  }

  # PRINT SUCCESS MESSAGE
  if (!silent) {
    message("SUCCESS: Center data saved for ", saveCount,
                " dates, ", sep="")

    message(failCount, " dates skipped (incomplete set)\n", sep="")
  }
  invisible(NULL)
}
