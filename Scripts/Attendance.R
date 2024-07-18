#### Attendance functions:

### attendanceCheck
attendanceCheck <- function(allowedBdays = 5)
{
  #Get list of dates any student attended
  uniDates <- unique(Update.Students(TRUE)$Last_Attendance_Date)
  
  #Create a lists of acceptable dates, which is based on parameter
  acceptableDates <- uniDates[order(uniDates, decreasing = TRUE)][c(
    1,allowedBdays)]
  
  
  flaggedStudents <- filter(Update.Students(TRUE), 
                            !between(Last_Attendance_Date, 
                                     acceptableDates[2],
                                     acceptableDates[1]) &
                              Delivery=="In-Center" &
                              Enrollment_Status == "Enrolled")
  return(flaggedStudents)
}#eof
