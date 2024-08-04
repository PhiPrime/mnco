getCenterData <- function(rawDataDir, date = Sys.Date(), ignoreMissing = F) {
  unmerged <- list()

  for (fileRoot in radiusFileRoots) {


    unmerged <- unmerged %>% append(dat)
  }

  # # Read and process raw data
  # students <- getStudentData(date, ignoreMissing)
  # accounts <- getAccountData(date, ignoreMissing)
  # progress <- getProgressData(date, ignoreMissing)
  # enrollments <- getEnrollmentData(date, ignoreMissing)
  #
  # # Merge into one data frame
  # # NEED TO EXAMINE MERGING
  # # MERGE getStudentRanking
  # all <- mergeWithFill(students, accounts, .by = "Account_Id")
  # all <- merge(all, progress, all.x = TRUE)
  # all <- merge(all, enrollments, all.x = TRUE)

  invisible(centerData)
}

as.rawFileName <- function(file_root, date = Sys.Date()){
  paste0(file_root, "  ", paste(month(date), day(date), year(date), sep = "_"),
         ".xlsx")
}
