#### Attendance functions:

### attendanceCheck
attendanceCheck <- function(allowedBdays = 5)
{
  #Get list of dates any student attended
  uniDates <- unique(Update.Students(TRUE)$Last_Attendance_Date)
  
  #Create a lists of acceptable dates, which is based on parameter
  acceptableDates <- uniDates[order(uniDates, decreasing = TRUE)][c(
    1,allowedBdays)]
  
  
  flaggedStudents <- filter(mergeWithFill(Update.Students(TRUE), 
                                          Update.Accounts(TRUE), 
                                          .by = "Account_Id"), 
                            !between(Last_Attendance_Date, 
                                     acceptableDates[2],
                                     acceptableDates[1]) &
                              Delivery=="In-Center" &
                              Enrollment_Status == "Enrolled") %>%
    #Select phone in this order: Mobile, Home, Other
    mutate(Phone = ifelse(is.na(Mobile_Phone), 
                          ifelse(is.na(Home_Phone), 
                                 Other_Phone, Home_Phone), Mobile_Phone)) %>%
        
    select(Last_Attendance_Date, First_Name, Last_Name, Account, Phone)
  
  
  flaggedStudents <- flaggedStudents[
    order(flaggedStudents$Last_Attendance_Date),]
  
  return(flaggedStudents)
}#eof
