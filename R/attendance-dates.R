getAttendanceDates <- function(days) {
  dates <- getAttendanceHistory()

  if (is.numeric(days)) {
    dates <- dates %>%
      filter(
        .data$Attendance_Date >= Sys.Date() - days,
        .data$Attendance_Date < Sys.Date()
      )
  } else if (days != "all") {
    stop("Invalid input for `days` parameter.")
  }

  dates <- dates %>%
    pull("Attendance_Date") %>%
    unique() %>%
    sort()

  return(dates)
}
