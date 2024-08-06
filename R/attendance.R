#######################     ATTENDANCE FUNCTIONS     ########################

attendanceCheck <- function(allowedBdays = 5)
{
  #Get list of dates any student attended
  uniDates <- unique(getStudentData()$Last_Attendance_Date)

  #Create a lists of acceptable dates, which is based on parameter
  acceptableDates <- uniDates[order(uniDates, decreasing = TRUE)][c(
    1,allowedBdays)]


  flaggedStudents <- filter(mergeWithFill(getStudentData(),
                                          getAccountData(),
                                          .by = "Account_Id"),
                            !between(Last_Attendance_Date,
                                     acceptableDates[2],
                                     acceptableDates[1]) &
                              Delivery=="In-Center" &
                              Enrollment_Status == "Enrolled") %>%

    transmute(Last_Attendance_Date = Last_Attendance_Date,
              Name = Student,
              #Select phone in this order: Mobile, Home, Other
              Phone = ifelse(is.na(Mobile_Phone),
                             ifelse(is.na(Home_Phone),
                                    Other_Phone, Home_Phone), Mobile_Phone),
              Link = cell_spec("Message", format = "latex",
                               link = paste0("./Cache/",
                                             asMessageTxtFile(Last_Attendance_Date,
                                                              Name))))



  flaggedStudents <- flaggedStudents[
    order(flaggedStudents$Last_Attendance_Date),]

  #Only send back those not on vacation
  flaggedStudents <-
    flaggedStudents[!(
      flaggedStudents$Name %in% getStudentsOnVacation()$Student),]

  return(flaggedStudents)
}#eof

########################     VACATION FUNCTIONS     #########################

## sendOnVacation
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
    addThisYear <- function(old) {lubridate::mdy(paste(
      old, lubridate::year(Sys.Date())))}
    returnDate <- tryCatch(
      expr = mdy(returnDate),
      error = function(e) {
        addThisYear(returnDate)
      },
      warning = function(w) {
        addThisYear(returnDate)
      }
    )
  }

  #Store current Student file for efficiency
  stus <- mutate(getStudentData(),
                 Student = Student)

  #Make function user friendly by regexing for name
  names <- stus$Student

  #Create data.frame to allow for sending multiple `who`s at the same time
  tmp <- data.frame(matrix(ncol = 1, nrow = 0, dimnames = list(NULL, "name")))
  for(i in who){
    tmp <- rbind(tmp,names[grepl(i, names, ignore.case = TRUE)])
  }

  who <- tmp[,1]
  stus <- filter(stus, Student%in%who)
  #Create data frame to store
  toStore <- data.frame(Student = who,
                        Last_Attendance = stus$Last_Attendance_Date,
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
  fileLoc <- paste0(getwd(), "/Cache/StudentsOnVacation", ".rds")

  #Query last attendance date
  dat <- merge(dat,
               mutate(getStudentData(),
                      Student = Student,
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
