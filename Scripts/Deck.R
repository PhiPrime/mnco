##########################     DECK FUNCTION    ###########################
### needsNewDeck
needsNewDeck <- function(minAllowed = 5, date=Sys.Date()){
  studentProgress <- getProgressData(date)
  #Set any NAs to 0
  studentProgress[
    is.na(studentProgress$Skills_Currently_Assigned),]$
    Skills_Currently_Assigned <- 0
  
  #Select students under minAllowed  
  ret <- filter(studentProgress, 
                Student %in% needsDeckBasedOnAssessment(date)|
                (Skills_Currently_Assigned < minAllowed &
                  Enrollment_Status == "Enrolled"))
  
  ret <- ret[order(ret$Skills_Currently_Assigned),]
  
  ret <- mutate(ret, Pest = Skills_Mastered/Attendances)
  ret <- select(ret, 
                Student, Skills_Currently_Assigned, Pest, 
                Skills_Mastered, Attendances)
  
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
  
  return()
}#eof

### getSuppressedStudents
getSuppressedStudents <- function(){
  fileLoc <- paste0(getwd(), "/Cache/suppressedStudents", ".rds")
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
  
  fileLoc <- paste0(getwd(), "/Cache/suppressedStudents", ".rds")
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
  fileLoc <- paste0(getwd(), "/Cache/suppressedStudents", ".rds")
  
  #Check for correct format
  if (any(names(studentRows) != correctNames)){
    stop(paste0("suppressDeckWarning(studentRows) passed incorrect argument. Data.Frame names should be: ", correctNames))
    
  }  
  
  dat <- getSuppressedStudents()
  dat <- dat[!dat$Student %in% studentRows$Student,]
  setSuppressedStudents(dat)
}#eof


