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
    mutate(Name = paste(First_Name, Last_Name),
           Phone = ifelse(is.na(Mobile_Phone), 
                          ifelse(is.na(Home_Phone), 
                                 Other_Phone, Home_Phone), Mobile_Phone)) %>%
        
    select(Last_Attendance_Date, Name, Account, Phone)
  
  
  flaggedStudents <- flaggedStudents[
    order(flaggedStudents$Last_Attendance_Date),]
  
  #Only send back those not on vacation
  flaggedStudents <- 
    flaggedStudents[!(
      flaggedStudents$Name %in% getStudentsOnVacation()$Student),]
  
  return(flaggedStudents)
}#eof

### sendOnVacation
# Sets a student to not appear in attendanceCheck(),
# should reappear if returnDate is reached or a new attendance occurs

## Overloaded form for attendanceCheck() param
# sendOnVacation <- function(attendanceRows = data.frame(
#   matrix(ncol=dim(attendanceCheck()), nrow = 0, 
#          dimnames = list(NULL, 
#                          names(attendanceCheck())))),
#   returnDate = Sys.Date()+lubridate::days(7)){
#   sendOnVacation(attendanceRows$Name, returnDate)
# }

##sendOnVacation standard
sendOnVacation <- function(who, 
                           
                           #Default end of current month
                           returnDate = lubridate::rollforward(Sys.Date())){
  
  #If returnDate is not in Date format
  if(!lubridate::is.Date(returnDate)){
    #Then try some formats
    returnDate <- mdy(returnDate)
    
    ## I am not familiar with R's tryCatch function. It seemed to be throwing
    ## errors from addThisYear(returnDate) even when mdy(returnDate) 
    ## successfully executed.
    
    #addThisYear <- function(old) {mdy(paste(old, lubridate::year(Sys.Date())))}
    # returnDate <- tryCatch(
    #   expr = mdy(returnDate), 
    #   error = addThisYear(returnDate),
    #   warning = addThisYear(returnDate))
  }
  
  #Make function user friendly by regexing for name
  names <- with(Update.Students(TRUE), paste(First_Name, Last_Name))
  who <-  names[grepl(who, names, ignore.case = TRUE)]
  
  #Create data frame to store
  toStore <- data.frame(Student = who,
                        returnDate = returnDate)
  
  if(dim(getStudentsOnVacation())[1]==0){
    dat <- toStore
  }
  else{
    dat <- getStudentsOnVacation()
    dat <- rbind(dat, toStore)
  }
  
  setStudentsOnVacation(dat)
  
}#eof

### getStudentsOnVacation
getStudentsOnVacation <- function(){
  fileLoc <- paste0(getwd(), "/Cache/StudentsOnVacation", ".rds")
  if(!file.exists(fileLoc)){
    #Run null constructor
    setStudentsOnVacation()
  }
  
  ret <- readRDS(fileLoc)
  
  #Update before returning (to check for exp)
  setStudentsOnVacation(ret)
  return(ret)
}

## setStudentsOnVacation
setStudentsOnVacation <- function(dat = data.frame(
  matrix(ncol=2, nrow = 0, 
         dimnames = list(NULL, 
                         c("Student", "returnDate"))))){
  #Query last attendance date
  dat <- merge(dat,
        mutate(Update.Students(TRUE),
         Student = paste(First_Name, Last_Name),
         Last_Attendance = Last_Attendance_Date) %>%
    select(Student, Last_Attendance))
  
  #Requirements for vacation
  ## They were not claimed to have returned,
  req1 <- which((Sys.Date()<dat$returnDate))
  
  ### Note: The following code is insufficient to see if a student
  ###        has attended since they were sent on Vacation. A restructure
  ###        of the stored datatype is needed. Current conditional should
  ###        be a tautology  
  
  ## They did not attend
  req2 <- which(dat$Last_Attendance<Sys.Date())
  
  
  #Only save those that meet requirements 
  dat <- dat[req1&req2,]
  
  fileLoc <- paste0(getwd(), "/Cache/StudentsOnVacation", ".rds")
  if(dim(dat)[1]==0){
    #Send warning about empty file
    warning(paste0("The following file is now empty:\n", fileLoc))
  }
  
  saveRDS(dat, fileLoc)
}

### returnStudentFromVacation
returnStudentFromVacation <- function(who){
  fileLoc <- paste0(getwd(), "/Cache/StudentsOnVacation", ".rds")
  
  #Check for correct format
  dat <- getStudentsOnVacation()
  
  dat <- dat[!grepl(who, dat$Student),]
  setStudentsOnVacation(dat)
}#eof
