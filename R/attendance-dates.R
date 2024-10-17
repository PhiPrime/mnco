getAttendanceDates <- function(days) {
  getCenterData("student") %>%
    filter(Last_Attendance_Date != Sys.Date()) %>%
    dplyr::pull("Last_Attendance_Date") %>%
    unique() %>%
    sort() %>%
    {if (days == "all") . else tail(., days)} %>%
    return()
}

# FIND A WAY TO CHECK WHEN ALL SHOULD BE RESAVED
saveAttendanceDates <- function(all = FALSE) {
  # Read cache and check if resave is needed
  cachePath <- cacheDir("attendanceDates.rds")

  # Save from today's data or choose to resave all
  if (!file.exists(cachePath) || all) {
    attendanceDates <- NULL
  } else {
    currentDates <- getCenterData("student") %>%
      dplyr::pull("Last_Attendance_Date") %>%
      unique() %>%
      as.Date()

    attendanceDates <- readRDS(cachePath) %>%
      union(currentDates)
  }

  # Resave all
  if (is.null(attendanceDates)) {
    dataDates <- list.files(rawDataDir()) %>%
      stringr::str_extract_all(
        "(?<=Students Export  )\\d{1,2}_\\d{1,2}_\\d{4}(?=\\.xlsx)"
      ) %>%
      unlist() %>%
      as.Date(format = "%m_%d_%Y")

    for (date in as.list(dataDates)) {
      data <- getCenterData("student", date = date) %>%
        dplyr::pull("Last_Attendance_Date") %>%
        unique() %>%
        as.Date()

      if (is.null(attendanceDates)) {
        attendanceDates <- data
      } else {
        attendanceDates <- attendanceDates %>%
          union(data)
      }
    }
  }

  # ADD IDENTICAL CHECK
  saveRDS(attendanceDates, cachePath)
}
