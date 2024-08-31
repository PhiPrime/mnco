#' Check which students need skills assigned
#'
#' @param minAllowed Threshold for number of assigned skills. Students under
#'  this threshold are returned.
#' @param date Date to use for data
#'
#' @return A data frame
#' @export
#'
#' @examples
#' # write later
needsNewDeck <- function(minAllowed = retrieve_variable("Deck_Minimum_Threshold"), date=Sys.Date()){
  studentProgress <- getCenterData("progress", date)
  #Set any NAs to 0
  studentProgress[
    is.na(studentProgress$Skills_Currently_Assigned),]$
    Skills_Currently_Assigned <- 0

  #Select students under minAllowed
  ret <- filter(
    studentProgress,
    .data$Student %in% needsDeckBasedOnAssessment(date) |
      (.data$Skills_Currently_Assigned < minAllowed &
         .data$Enrollment_Status == "Enrolled")
  )

  ret <- ret[order(ret$Skills_Currently_Assigned),]

  ret <- mutate(ret, Pest = .data$Skills_Mastered/.data$Attendances)
  ret <- select(ret,
                       "Student", "Skills_Currently_Assigned", "Pest",
                       "Skills_Mastered", "Attendances")

  #Check for suppressed students and remove if so
  dat <- getSuppressedStudents()
  ret <- ret[!ret$Student %in% dat$Student,]
  ret <- ret[order(ret$Student),]
  return(ret)
}

### suppressDeckWarning
#' Exclude student from deck warnings
#'
#' @param studentRows Data frame of students
#' @param durationDays Number of days
#'
#' @return None (invisible `NULL`)
#' @export
#'
#' @examples
#' # write later
suppressDeckWarning <- function(studentRows = data.frame(
  matrix(ncol=5, nrow = 0,
         dimnames = list(NULL,
                         c("Student", "Skills_Currently_Assigned", "Pest",
                           "Skills_Mastered", "Attendances")))),
  durationDays = retrieve_variable("Deck_Warning_Duration")){
  ###
  # Add readline() commands and a while loop to make friendly UI to quickly
  # suppress students

  #Don't allow suppression longer than `maxTime` days
  maxTime <- retrieve_variable("Deck_Suppression_Maximum_Time")

  if(durationDays > maxTime){ durationDays <- maxTime}
  expDate <- Sys.Date()+lubridate::days(durationDays)
  correctNames <- c("Student", "Skills_Currently_Assigned", "Pest",
                    "Skills_Mastered", "Attendances")

  if (any(names(studentRows) != correctNames)){

    error <- paste0(
      "suppressDeckWarning(studentRows) passed incorrect argument.\n",
      "Data.Frame names should be:\n\t", paste0(correctNames,
                                                collapse = "", sep="\n\t"))
    stop(error)

  }


  #Add columns for both created & expiration date
  studentRows <- mutate(studentRows, creation = Sys.Date(),
                               expDate = expDate)
  if(dim(getSuppressedStudents())[1]==0){
    dat <- studentRows
  }
  else{
    dat <- getSuppressedStudents()
    dat <- rbind(dat, studentRows)
  }
  setSuppressedStudents(dat)

  invisible(NULL)
}

### getSuppressedStudents
#' Retrieve list of suppressed students
#'
#' @return A data frame
#' @export
#'
#' @examples
#' # write later
getSuppressedStudents <- function(){
  fileLoc <- file.path(cacheDir(), "suppressions.rds")
  if(!file.exists(fileLoc)){
    #Run null constructor
    setSuppressedStudents()
  }

  ret <- readRDS(fileLoc)

  #Order the values
  ret <- ret[order(ret$expDate, decreasing = FALSE),]

  #Update before returning
  setSuppressedStudents(ret)


  return(ret)
}

#' (to be deprecated) Save students to suppression file
#'
#' @param dat Data frame of students
#'
#' @return None (invisible `NULL`)
#'
setSuppressedStudents <- function(dat = data.frame(
  matrix(ncol=7, nrow = 0,
         dimnames = list(NULL,
                         c("Student", "Skills_Currently_Assigned", "Pest",
                           "Skills_Mastered", "Attendances",
                           "creation", "expDate"))))){
  #Check for expired stints
  dat <- dat[which((Sys.Date()<dat$expDate)),]

  fileLoc <- file.path(cacheDir(), "suppressions.rds")
  saveRDS(dat, fileLoc)

  invisible(NULL)
}

### removeDeckSuppression
#' Remove deck warning suppression from student
#'
#' @param studentRows Data frame of students
#'
#' @return None (invisible(`NULL`))
#' @export
#'
#' @examples
#' # write later
removeDeckSuppression <- function(studentRows = data.frame(
  matrix(ncol=7, nrow = 0,
         dimnames = list(NULL,
                         c("Student", "Skills_Currently_Assigned", "Pest",
                           "Skills_Mastered", "Attendances",
                           "creation", "expDate"))))){

  correctNames <- c("Student", "Skills_Currently_Assigned", "Pest",
                    "Skills_Mastered", "Attendances",
                    "creation", "expDate")
  fileLoc <- file.path(cacheDir(), "suppressions.rds")

  #Check for correct format
  if (any(names(studentRows) != correctNames)){
    stop(paste0("suppressDeckWarning(studentRows) passed incorrect argument. Data.Frame names should be: ", correctNames))

  }

  dat <- getSuppressedStudents()
  dat <- dat[!dat$Student %in% studentRows$Student,]
  setSuppressedStudents(dat)

  invisible(NULL)
}
