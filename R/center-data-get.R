#' Get tidied data from Radius files
#'
#' @param date What date to get data for.
#' @param ignoreMissing Whether to throw an error if file is not found
#' @param dir Directory containing Radius files
#' @param type Which kind of Radius data to get
#'
#' @return A data frame
#' @export
#'
#' @examples
getCenterData <- function(type = c("all", names(RADIUS_FILE_ROOTS)),
                          date = Sys.Date(), ignoreMissing = F) {
  type <- match.arg(type)

  # USE match.arg, stopifnot
  if (type == "all") {
    # Get all data and merge
    stu <- getCenterData()
  } else if (type %in% names(RADIUS_FILE_ROOTS)) {
    # Get and tidy data
    data <-
      readRawData(type, date) %>%
      tidyRawData(type)
  } else {
    stop("`type` is not a valid argument: \'", type, "\'")
  }

  invisible(data)
}

getPaymentData <- function(date = Sys.Date(), ignoreMissing = F) {
  dat <- readRawData.old("Payments.xlsx", date)

  return(dat)
}#eof

### getCurriculumData
getCurriculumData <- function(date = Sys.Date(), ignoreMissing = F) {
  dat <- readRawData.old("Curriculum Library Export", date)

  return(dat)
}

### getAttendanceHistory
getAttendanceTrainingSet <- function() {
  readRDS(file.path(cacheDir(), "prior2024.rds"))
}#eof

### getAttendanceData
getAttendanceData <- function(get = FALSE, date = Sys.Date()) {
  dat <- readRawData.old("Student Attendance Report Export", date)

  logfile <- file.path(cacheDir(), "studentAttendanceLog.csv")

  if(!file.exists(logfile)) {
    stop("While running getAttendanceData(), \"", logfile,
         "\" was not found.")
  } else {
    logdat <- read.csv(logfile)
  }

  #Mutate to tidy
  dat <- dplyr::mutate(dat,
                date = as.Date(Attendance_Date,
                               format = "%m/%d/%y"),
                accountID = Account_Id,
                name = paste(First_Name,Last_Name),
                startTime = strptime(Arrival_Time, "%I:%M %p"),
                endTime = strptime(Departure_Time, "%I:%M %p"),
                totalVisits = Total_Visits,
                membershipType = as.factor(Membership_Type),
                sessionsPerMonth = as.factor(Sessions_Per_Month),
                sessionsRemaining = Sessions_Remaining,
                delivery = as.factor(Delivery))

  dat <- dplyr::mutate(dat,
                line = paste(accountID,date,name,
                             sep = ";"))

  dat <- dplyr::select(dat, date:delivery, line)


  newdat <- dat[!(dat$line %in% logdat$line),]

  #Check for and notify if no new data is found
  if(dim(newdat)[1]==0){
    message(cat("NOTICE: getAttendanceData(get = ", get, ", date = ", as.character(date),
                ") found no new attendance when updating.", sep = ""))
  }

  else {
    # write.csv()
  }


  #Save to File
  write.csv(logdat,logfile, row.names=FALSE)

  if(get){
    return(read.csv(logfile))
  }

  ### Old code to check for two types of files. Could be useful to
  ### convert stored data from xlsx to csv for better longterm storage.
  # if(grepl("xlsx$", fileLoc)) {
  #   newdat <- read_xlsx(fileLoc)
  # } else if (grepl("csv$", fileLoc)) {
  #   newdat <- read.csv(fileLoc)
  # } else {
  #   stop(paste0("While running getAttendanceData ", fileLoc,
  #               "was not able to be read."))
  # }


}#eof
