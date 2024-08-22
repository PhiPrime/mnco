#' Check which students have not attended
#'
#' @param allowedBdays Minimum number of days not attended to flag
#'
#' @return A data frame
#' @export
#'
#' @examples
#'  )
#' attendanceCheck(allowedBdays = 10)
attendanceCheck <- function(allowedBdays = retrieve_variable("Attendance_Allowed_Days")) {
  #Get list of dates any student attended
  uniDates <- unique(getCenterData("student")$Last_Attendance_Date)

  #Create a lists of acceptable dates, which is based on parameter
  acceptableDates <- uniDates[order(uniDates, decreasing = TRUE)][c(
    1,allowedBdays)]


  flaggedStudents <- dplyr::filter(mergeWithFill(getCenterData("student"),
                                          getCenterData("account"),
                                          .by = "Account_Id"),
                            !dplyr::between(.data$Last_Attendance_Date,
                                     acceptableDates[2],
                                     acceptableDates[1]) &
                              .data$Enrollment_Status == "Enrolled") %>%

    dplyr::transmute(Last_Attendance_Date = .data$Last_Attendance_Date,
              Name = .data$Student,
              Account = paste(stringr::str_remove(.data$Account, "^.+, "),
                              stringr::str_remove(.data$Account, ",.+$")),
              #Select phone in this order: Mobile, Home, Other
              Phone = ifelse(is.na(.data$Mobile_Phone),
                             ifelse(is.na(.data$Home_Phone),
                                    .data$Other_Phone, .data$Home_Phone),
                             .data$ Mobile_Phone))
  #Create & Populate text files with completed templates
  tmpLink <- function(date, name) {
    paste0(cacheDir(),"/Messages/",
           asMessageTxtFile(.data$Last_Attendance_Date,
                            .data$Name))
  }

  #sapply(flaggedStudents)

  flaggedStudents <- dplyr::mutate(flaggedStudents,
                            d = kableExtra::cell_spec("Message", format = "latex",
                               link = tmpLink(.data$Last_Attendance_Date,
                                                              .data$Name)))
  # JUSTIN'S TEST
  flaggedStudents <- dplyr::mutate(flaggedStudents,
                            Link = kableExtra::cell_spec("Message", format = "latex",
                                                         link = "../mcp-data/daily-report.pdf"))



  flaggedStudents <- flaggedStudents[
    order(flaggedStudents$Last_Attendance_Date),]

  #Only send back those not on vacation
  flaggedStudents <-
    flaggedStudents[!(
      flaggedStudents$Name %in% getStudentsOnVacation()$Student),]

  return(flaggedStudents)
}

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

#' Send student on vacation
#'
#' @param who Student name
#' @param returnDate Date of return
#'
#' @return None (invisible `NULL`)
#' @export
#'
#' @examples
#' sendOnVacation("John Doe", returnDate = "2024-07-31")
sendOnVacation <- function(who, returnDate = lubridate::rollforward(Sys.Date())) {

  #If returnDate is not in Date format
  if(!lubridate::is.Date(returnDate)){
    #Then try some formats
    addThisYear <- function(old) {lubridate::mdy(paste(
      old, lubridate::year(Sys.Date())))}
    returnDate <- tryCatch(
      expr = lubridate::mdy(returnDate),
      error = function(e) {
        addThisYear(returnDate)
      },
      warning = function(w) {
        addThisYear(returnDate)
      }
    )
  }

  #Store current Student file for efficiency
  stus <- dplyr::mutate(getCenterData("student"),
                 Student = .data$Student)

  #Make function user friendly by regexing for name
  names <- stus$Student

  #Create data.frame to allow for sending multiple `who`s at the same time
  tmp <- data.frame(matrix(ncol = 1, nrow = 0, dimnames = list(NULL, "name")))
  for(i in who){
    tmp <- rbind(tmp,names[grepl(i, names, ignore.case = TRUE)])
  }

  who <- tmp[,1]
  stus <- dplyr::filter(stus, .data$Student%in%who)
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
  invisible(NULL)

}

#' Get students on vacation
#'
#' @return A data frame
#' @export
#'
#' @examples
#' getStudentsOnVacation()
getStudentsOnVacation <- function(){
  fileLoc <- file.path(cacheDir(), "StudentsOnVacation.rds")
  if(!file.exists(fileLoc)){
    #Run null constructor
    setStudentsOnVacation()
  }

  ret <- readRDS(fileLoc)

  #Update before returning (to check for exp)
  setStudentsOnVacation(ret)
  return(ret)
}

#' Add student to vacation file
#'
#' @param dat Data frame of students to add
#'
#' @return None (invisible `NULL`)
#' @export
#'
#' @examples
#' setStudentsOnVacation()
setStudentsOnVacation <- function(dat = data.frame(
  matrix(ncol=2, nrow = 0,
         dimnames = list(NULL,
                         c("Student", "returnDate"))))){
  fileLoc <- file.path(cacheDir(), "StudentsOnVacation.rds")

  #Query last attendance date
  dat <- merge(dat,
               dplyr::mutate(getCenterData("student"),
                      Student = .data$Student,
                      Last_Attendance = .data$Last_Attendance_Date) %>%
                 dplyr::select("Student", "Last_Attendance"))

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
  invisible(NULL)
}

#' Remove student from vacation file
#'
#' @param who Student to remove
#'
#' @return None (invisible `NULL`)
#' @export
#'
#' @examples
#' returnStudentFromVacation("John Doe")
returnStudentFromVacation <- function(who){
  fileLoc <- file.path(cacheDir(), "StudentsOnVacation.rds")

  #Check for correct format
  dat <- getStudentsOnVacation()

  dat <- dat[!grepl(who, dat$Student),]
  setStudentsOnVacation(dat)
}#eof
