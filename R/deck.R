##########################     DECK FUNCTION    ###########################
### needsNewDeck
#' Title
#'
#' @param minAllowed
#' @param date
#'
#' @return
#' @export
#'
#' @examples
needsNewDeck <- function(minAllowed = 5, date=Sys.Date()){
  studentProgress <- getCenterData("progress", date)
  #Set any NAs to 0
  studentProgress[
    is.na(studentProgress$Skills_Currently_Assigned),]$
    Skills_Currently_Assigned <- 0

  #Select students under minAllowed
  ret <- dplyr::filter(studentProgress,
                       .data$Student %in% needsDeckBasedOnAssessment(date)|
                         (.data$Skills_Currently_Assigned < minAllowed &
                            .data$Enrollment_Status == "Enrolled"))

  ret <- ret[order(ret$Skills_Currently_Assigned),]

  ret <- dplyr::mutate(ret, Pest = .data$Skills_Mastered/.data$Attendances)
  ret <- dplyr::select(ret,
                       "Student", "Skills_Currently_Assigned", "Pest",
                       "Skills_Mastered", "Attendances")

  #Check for suppressed students and remove if so
  dat <- getSuppressedStudents()
  ret <- ret[!ret$Student %in% dat$Student,]
  ret <- ret[order(ret$Student),]
  return(ret)
}#eof

#######################     SUPPRESSION FUNCTIONS     #######################
### suppressDeckWarning
suppressDeckWarning <- function(studentRows = data.frame(
  matrix(ncol=5, nrow = 0,
         dimnames = list(NULL,
                         c("Student", "Skills_Currently_Assigned", "Pest",
                           "Skills_Mastered", "Attendances")))),
  durationDays = 2){
  ###
  # Add readline() commands and a while loop to make friendly UI to quickly
  # suppress students

  #Don't allow suppression longer than `maxTime` days
  maxTime <- 30

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
  studentRows <- dplyr::mutate(studentRows, creation = Sys.Date(),
                               expDate = expDate)
  if(dim(getSuppressedStudents())[1]==0){
    dat <- studentRows
  }
  else{
    dat <- getSuppressedStudents()
    dat <- rbind(dat, studentRows)
  }
  setSuppressedStudents(dat)

  return()
}#eof

### getSuppressedStudents
getSuppressedStudents <- function(){
  fileLoc <- file.path(cacheDir(), "suppressedStudent.rds")
  if(!file.exists(fileLoc)){
    #Run null constructor
    setSuppressedStudents()
  }

  ret <- readRDS(fileLoc)
  #Update before returning
  setSuppressedStudents(ret)
  return(ret)
}

setSuppressedStudents <- function(dat = data.frame(
  matrix(ncol=7, nrow = 0,
         dimnames = list(NULL,
                         c("Student", "Skills_Currently_Assigned", "Pest",
                           "Skills_Mastered", "Attendances",
                           "creation", "expDate"))))){
  #Check for expired stints
  dat <- dat[which((Sys.Date()<dat$expDate)),]

  fileLoc <- file.path(cacheDir(), "suppressedStudent.rds")
  saveRDS(dat, fileLoc)
}

### removeDeckSuppression
removeDeckSuppression <- function(studentRows = data.frame(
  matrix(ncol=7, nrow = 0,
         dimnames = list(NULL,
                         c("Student", "Skills_Currently_Assigned", "Pest",
                           "Skills_Mastered", "Attendances",
                           "creation", "expDate"))))){

  correctNames <- c("Student", "Skills_Currently_Assigned", "Pest",
                    "Skills_Mastered", "Attendances",
                    "creation", "expDate")
  fileLoc <- file.path(cacheDir(), "suppressedStudents.rds")

  #Check for correct format
  if (any(names(studentRows) != correctNames)){
    stop(paste0("suppressDeckWarning(studentRows) passed incorrect argument. Data.Frame names should be: ", correctNames))

  }

  dat <- getSuppressedStudents()
  dat <- dat[!dat$Student %in% studentRows$Student,]
  setSuppressedStudents(dat)
}#eof

#Used with ranking to adjust score based on variableName
regularizeScore <- function(dat, variableName, centerVal){
  #if no Score present in data frame, assume it should be LB
  if(!("Score" %in% names(dat))){
    dat <- dplyr::mutate(dat, Score = .data$LB)
  }

  gdMeans <- t(sapply(unique(dat[[variableName]]), function(x)
    data.frame(variableName=x,
               mean= mean(dat[dat[[variableName]]==x,]$Pest))))
  gdMeans <- data.frame(tmp = unlist(gdMeans[,1]),
                        Mean = unlist(gdMeans[,2]))
  names(gdMeans)[names(gdMeans) == "tmp"] <- variableName

  gdMeans <- gdMeans[order(gdMeans[[variableName]]),]
  gdMeans <- dplyr::mutate(gdMeans,
                           offset = gdMeans[gdMeans[[variableName]]==centerVal,
                           ]$Mean-.data$Mean)
  dat <- merge(dat, gdMeans)
  dat$Score <- with(dat, Score + offset)
  dat <- dplyr::select(dat, -"offset", -"Mean")
  return(dat)
}

showcaseRegularizeScore <- function(){
  dat <- merge(getStudentRanking(),
               getMostRecentAssessments())

  #Force -3 to be min difference considered
  dat[which(dat$gradeDif<(-3)),]$gradeDif <- -3

  p1 <- ggplot2::ggplot(dat, ggplot2::aes(x=.data$gradeDif, y = .data$LB)) +
    ggplot2::ylab("Score") +
    ggplot2::geom_point() + ggplot2::geom_smooth() + ggplot2::ggtitle("No Regularization")


  dat <- regularizeScore(dat,"Level", 4)#  "gradeDif", 0)

  p2 <- ggplot2::ggplot(dat, ggplot2::aes(x=.data$gradeDif, y = .data$Score)) +
    ggplot2::geom_point() + ggplot2::geom_smooth() + ggplot2::ggtitle("Regularized on gradeDif")


  dat <- regularizeScore(dat,  "gradeDif", 0)#"Level", 4)

  p3 <- ggplot2::ggplot(dat, ggplot2::aes(x=.data$gradeDif, y = .data$Score)) +
    ggplot2::geom_point() + ggplot2::geom_smooth() +
    ggplot2::ggtitle("Regularized on gradeDif & assessmentLevel")


  gridExtra::grid.arrange(p1,p2,p3,ncol=1)
  #return(dat)
}
