#' Read and tidy Radius data
#'
#' @param date Date to read data for
#' @param ignoreMissing `logical` indicating to return empty data frame
#'  (instead of signaling an error) if data file not found
#' @param type String of which Radius data to read
#'
#' @return A data frame
#' @export
#'
#' @examples
#' getCenterData()
#' getCenterData("student")
getCenterData <- function(type = c("all", radiusFileRoots("types")),
                          date = Sys.Date(), ignoreMissing = F) {
  # Ensure valid type of Radius data file
  type <- match.arg(type)

  # USE match.arg, stopifnot
  if (type == "all") {
    # Get all data and merge
    stu <- getCenterData("student", date)
    acc <- getCenterData("account", date)
    pro <- getCenterData("progress", date)
    enr <- getCenterData("enrollment", date)

    tdat <- stu %>%
      mergeWithFill(acc, .by = "Account_Id") %>%
      merge(pro, all.x = T) %>%
      merge(enr, all.x = T)

  } else {
    # Get and tidy data
    tdat <- readRawData(type, date) %>% tidyRawData(type)
  }

  invisible(tdat)
}

### getAttendanceData
#' Title
#'
#' @param get `logical` indicating to return attendance log file
#' @param date
#'
#' @return A data frame if `get` is `TRUE`, otherwise none (invisible `NULL`)
#' @export
#'
#' @examples
#' getAttendanceData()
getAttendanceData <- function(get = FALSE, date = Sys.Date()) {
  dat <- readRawData("attendance", date)

  logfile <- file.path(cacheDir(), "studentAttendanceLog.csv")

  if(!file.exists(logfile)) {
    stop("While running getAttendanceData(), \"", logfile,
         "\" was not found.")
  } else {
    logdat <- utils::read.csv(logfile)
  }

  #Mutate to tidy
  dat <- dplyr::mutate(dat,
                date = as.Date(.data$Attendance_Date,
                               format = "%m/%d/%y"),
                accountID = .data$Account_Id,
                name = paste(.data$First_Name,.data$Last_Name),
                startTime = strptime(.data$Arrival_Time, "%I:%M %p"),
                endTime = strptime(.data$Departure_Time, "%I:%M %p"),
                totalVisits = .data$Total_Visits,
                membershipType = as.factor(.data$Membership_Type),
                sessionsPerMonth = as.factor(.data$Sessions_Per_Month),
                sessionsRemaining = .data$Sessions_Remaining,
                delivery = as.factor(.data$Delivery))

  dat <- dplyr::mutate(dat,
                line = paste(.data$accountID,.data$date,.data$name,
                             sep = ";"))

  dat <- dplyr::select(dat, "date":"delivery", "line")


  newdat <- dat[!(dat$line %in% logdat$line),]

  #Check for and notify if no new data is found
  if(dim(newdat)[1]==0){
    message("NOTICE: getAttendanceData(get = ", get, ", date = ", as.character(date),
                ") found no new attendance when updating.", sep = "")
  }

  else {
    # utils::write.csv()
  }


  #Save to File
  utils::write.csv(logdat,logfile, row.names=FALSE)

  if(get){
    return(utils::read.csv(logfile))
  }

  ### Old code to check for two types of files. Could be useful to
  ### convert stored data from xlsx to csv for better longterm storage.
  # if(grepl("xlsx$", fileLoc)) {
  #   newdat <- read_xlsx(fileLoc)
  # } else if (grepl("csv$", fileLoc)) {
  #   newdat <- utils::read.csv(fileLoc)
  # } else {
  #   stop(paste0("While running getAttendanceData ", fileLoc,
  #               "was not able to be read."))
  # }

  invisible(NULL)
}

# these functions are intended ONLY for convenience in interactive testing with
#   load_all() (or ctrl+shift+L)
# do NOT use these in a package function. use otherwise at risk of them being
#   removed at any time
cstu <- function() getCenterData("student")
cacc <- function() getCenterData("account")
cenr <- function() getCenterData("enrollment")
cpro <- function() getCenterData("progress")
