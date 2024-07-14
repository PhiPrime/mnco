#### Deck functions:

### needsNewDeck
needsNewDeck <- function(minAllowed = 5){
  Update.Progress()
  #Set any NAs to 0
  studentProgress[
    is.na(studentProgress$Skills_Currently_Assigned),]$
    Skills_Currently_Assigned <- 0
  
  #Select students under minAllowed  
  ret <- filter(studentProgress, 
                Skills_Currently_Assigned < minAllowed &
                  Enrollment_Status == "Enrolled")
  
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

### suppressDeckWarning
suppressDeckWarning <- function(studentRows = data.frame(
  matrix(ncol=5, nrow = 0, 
         dimnames = list(NULL, 
                         c("Student", "Skills_Currently_Assigned", "Pest", 
                           "Skills_Mastered", "Attendances")))),
  durationDays = 2){
  # 
  # Add readline() commands and a while loop to make friendly UI to quickly
  # suppress students
  
  #Don't allow suppression longer than `maxTime` days
  maxTime <- 30
  
  if(durationDays > maxTime){ durationDays <- maxTime}
  expDate <- Sys.Date()+days(durationDays)
  correctNames <- c("Student", "Skills_Currently_Assigned", "Pest", 
                    "Skills_Mastered", "Attendances")
  
  if (any(names(studentRows) != correctNames)){
    
    print(paste0("Error: suppressDeckWarning(studentRows) passed incorrect argument. Data.Frame names should be: ", correctNames))
    stop()
    
  } 
  
  
  #Add columns for both created & expiration date
  studentRows <- mutate(studentRows, creation = today(),
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
  fileLoc <- paste0(getwd(), "/Data/suppressedStudents", ".csv")
  if(!file.exists(fileLoc)){
    #Run null constructor
    setSuppressedStudents()
  }
  
  ret <- read.csv(fileLoc)
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
  dat <- dat[which((today()<dat$expDate)),]
  
  fileLoc <- paste0(getwd(), "/Data/suppressedStudents", ".csv")
  if(dim(dat)[1]==0){
    
    #Prompt file is about to be empty and check to continue
    ans <- readline(prompt = paste0("Notice: \n", fileLoc,
                                    " \nis about to be empty, continue? (Y/N):"))
    if(grepl("[Nn]", ans)){ stop() }
  }
  
  
  write.csv(dat, fileLoc, row.names = FALSE)
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
  fileLoc <- paste0(getwd(), "suppressedStudents", ".csv")
  
  #Check for correct format
  if (any(names(studentRows) != correctNames)){
    stop(paste0("suppressDeckWarning(studentRows) passed incorrect argument. Data.Frame names should be: ", correctNames))
    
  }  
  
  dat <- getSuppressedStudents()
  dat <- dat[!dat$Student %in% studentRows$Student,]
  setSuppressedStudents(dat)
}#eof

moveDataDownloads <- function(fileNames) {
  rmPath <- gsub("^.*[/].*[/].*[/].*?", "", getwd())
  downloadPath <- paste0(gsub(rmPath, "", getwd()), "Downloads/")
  filePaths <- paste0(downloadPath, fileNames)
  
  if(!grepl("Overview$", getwd())) {
    stop("while trying to moveDataDownloads,\n",
         getwd(), "\nis the working directory but does not\n",
         "end with \"Overview\"")
  }
  
  fileDests <- paste0(getwd(), "/Data/", fileNames)
  
  for(i in 1:length(filePaths)){
    if(file.exists(filePaths[i])){
      file.rename(filePaths[i], fileDests[i])
      print(paste0(filePaths[i], "-- moved to --> ", fileDests[i]))
    } else {
      print(paste0("Notice: the file ", filePaths[i],
                   " could not be found."))
    }
  }
  
}#eof
