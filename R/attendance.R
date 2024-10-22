#' @export
getAttendanceHistory <- function() {
  cachePath <- cacheDir("attendanceHistory.rds")

  if (!file.exists(cachePath)) {
    saveAttendanceHistory(all = T)
  }

  cache <- readRDS(cachePath)

  return(cache)
}

#' @export
saveAttendanceHistory <- function(all = F) {
  if (all)
    saveAttendanceHistory.all()
}

saveAttendanceHistory.all <- function() {
  attendanceDataDates <- list.files(rawDataDir()) %>%
    stringr::str_extract_all(
      "(?<=Student Attendance Report Export  )\\d{1,2}_\\d{1,2}_\\d{4}(?=\\.xlsx)"
    ) %>%
    unlist() %>%
    as.Date(format = "%m_%d_%Y") %>%
    sort()

  history <- rawDataDir("Attendance History 2022 to 10_21_2024.xlsx") %>%
    {readRawData("path", path = .)} %>%
    tidyRawData.attendance()

  for (date in as.list(attendanceDataDates)) {
    data <- getCenterData("attendance", date = date)

    history <- history %>%
      dplyr::rows_upsert(data, by = c("Student", "Attendance_Date"))
  }

  history <- history %>%
    dplyr::arrange(.data$Student, .data$Attendance_Date) %>%
    saveRDS(cacheDir("attendanceHistory.rds"))
}
