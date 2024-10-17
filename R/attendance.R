#' @export
getAttendanceHistory <- function() {
  cachePath <- cacheDir("attendanceHistory.rds")

  if (!file.exists(cachePath)) {
    saveAttendanceHistory(all = T)
  }
}

#' @export
saveAttendanceHistory <- function(all = F) {
  if (all) saveAttendanceHistory.all
}

saveAttendanceHistory.all <- function() {
  history <- rawDataDir("Attendance Report 2022 to 10_8_2024.xlsx") %>%
    {readRawData("path", path = .)} %>%
    tidyRawData.attendance()

  attendanceDataDates <- list.files(rawDataDir()) %>%
    stringr::str_extract_all(
      "(?<=Student Attendance Report Export  )\\d{1,2}_\\d{1,2}_\\d{4}(?=\\.xlsx)"
    ) %>%
    unlist()

  for (date in attendanceDataDates) {
    data <- getCenterData("attendance", date = date)
  }
}
