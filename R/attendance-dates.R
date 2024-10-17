attendanceDates <- function(days) {
  getCenterData("student") %>%
    filter(Last_Attendance_Date != Sys.Date()) %>%
    dplyr::pull("Last_Attendance_Date") %>%
    unique() %>%
    sort() %>%
    {if (days == "all") . else tail(., days)} %>%
    return()
}

saveAttendanceDates <- function() {

}
