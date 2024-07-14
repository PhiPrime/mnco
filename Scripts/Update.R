#### Update functions:

### Update.All
Update.All <- function(get = FALSE){
  # Read excel files
  accounts <- Update.Accounts(TRUE)
  students <- Update.Students(TRUE)
  progress <- Update.Progress(TRUE)
  #payments <- Update.Payments(TRUE)
  #payments <- mutate(payments, Account = Account_Name)
  enrollments <- Update.Enrollments(TRUE)
  
  # Add columns
  students <- mutate(students, Student = paste0(First_Name, " ", Last_Name))
  
  # Merge into one data frame
  all <- merge(students, accounts, all.x = TRUE)
  all <- merge(all, progress, all.x = TRUE)
  #all <- merge(all, payments, all.x = TRUE)
  all <- merge(all, enrollments, all.x = TRUE)
  
  if(get) {
    # Return data frame as getter
    return(all)
  } else {
    # Split by enrollment status
    active <- filter(all, Enrollment_Status == "Enrolled")
    inactive <- filter(all, Enrollment_Status != "Enrolled")
    
    # Assign to global environment
    assign("active",
           filter(all, Enrollment_Status == "Enrolled"),
           envir = .GlobalEnv)
    assign("inactive",
           filter(all, Enrollment_Status != "Enrolled"),
           envir = .GlobalEnv)
  }
}

### Update.Accounts
Update.Accounts <- function(get = FALSE, date = Sys.Date()){
  fileName <- paste0("Account Export  ", as.radiusDate(date), ".xlsx")
  filePath <- file.path(dataDir, fileName)
  
  if(!file.exists(filePath)) {
    #Try to move fileName from downloads
    moveDataDownloads(fileName)
    if(!file.exists(filePath)){
      stop(paste0("Up to date \n\"", filePath, "\"\nnot found"))
    }
  }
  #Implied else, file must exists
  dat <- read_excel(filePath, .name_repair = "unique_quiet")
  names(dat) <- gsub(" ", "_", names(dat))
  
  
  accounts <- filter(dat, Enrollment_Status == "Active")
  inactive <- filter(dat, Enrollment_Status == "Inactive")
  
  if(get) {
    return(dat)
  } else {
    assign("accounts",accounts,envir = .GlobalEnv)
    assign("inactiveAccounts",inactive,envir = .GlobalEnv)
  }
}#eof

### Update.Students
Update.Students <- function(get = FALSE, date = Sys.Date()){
  fileName <- paste0("Students Export  ", as.radiusDate(date), ".xlsx")
  filePath <- file.path(dataDir, fileName)
  
  if(!file.exists(filePath)) {
    #Try to move fileName from downloads
    moveDataDownloads(fileName)
    if(!file.exists(filePath)){
      stop(paste0("Up to date \n\"", filePath, "\"\nnot found"))
    }
  }
  #Implied else, file must exists
  dat <- read_excel(filePath, .name_repair = "unique_quiet")
  names(dat) <- gsub(" ", "_", names(dat))
  dat <- mutate(dat, 
                Last_Attendance_Date = mdy(Last_Attendance_Date))
  #"failed to parse" warning gets thrown
  
  students <- filter(dat, Enrollment_Status == "Enrolled")
  inactiveStudents <- filter(dat, Enrollment_Status != "Enrolled")
  
  if(get){
    return(dat)
  } else {
    assign("students",students,envir = .GlobalEnv)
    assign("inactiveStudents",inactiveStudents,envir = .GlobalEnv)
  }
}#eof

### Update.Progress
Update.Progress <- function(get = FALSE, date = Sys.Date()) {
  fileName <- paste0("Current Batch Detail Export  ", as.radiusDate(date), ".xlsx")
  filePath <- file.path(dataDir, fileName)
  
  if(!file.exists(filePath)) {
    #Try to move fileName from downloads
    moveDataDownloads(fileName)
    if(!file.exists(filePath)){
      stop(paste0("Up to date \n\"", filePath, "\"\nnot found"))
    }
  }
  #Implied else, file must exists
  dat <- read_excel(filePath, .name_repair = "unique_quiet")
  names(dat) <- gsub(" ", "_", names(dat))
  
  tdat <- dat[which(!dat$Attendances<5 & !dat$Skills_Mastered<2),]
  tdat <- mutate(tdat, Pest = Skills_Mastered/Attendances)
  tdat <- mutate(tdat, 
                 UB = round(Pest-qnorm((1-95/100)/2)*
                              sd(tdat$Pest)/sqrt(Attendances),5),
                 LB = round(Pest+qnorm((1-95/100)/2)*
                              sd(tdat$Pest)/sqrt(Attendances),5))
  tdat <- mutate(tdat, fontsize = round(32*LB/max(tdat$LB), 1))
  tdat <- tdat[order(tdat$LB, decreasing = TRUE),]
  tdat <- mutate(tdat, Rank = dim(tdat)[1] +1 - rank(LB, ties.method = "max"))
  
  if(get) {
    return(dat)
  } else {
    assign("studentProgress",dat,envir = .GlobalEnv)
    assign("selectStudentProgress", tdat, envir = .GlobalEnv)
    assign("studentRanking", select(tdat, 
                                    Rank, Student, fontsize, UB, Pest, LB, 
                                    Skills_Mastered, Attendances), envir = .GlobalEnv)
  }
}#eof


### Update.Payments
Update.Payments <- function(get = FALSE, date = Sys.Date()){
  # yes the file name is stupid
  fileName <- paste0("Payments.xlsx  ", as.radiusDate(date), ".xlsx")
  filePath <- file.path(dataDir, fileName)
  
  if(!file.exists(filePath)) {
    #Try to move fileName from downloads
    moveDataDownloads(fileName)
    if(!file.exists(filePath)){
      stop(paste0("Up to date \n\"", filePath, "\"\nnot found"))
    }
  }
  #Implied else, file must exists
  dat <- read_excel(filePath, .name_repair = "unique_quiet")
  names(dat) <- gsub(" ", "_", names(dat))
  
  if(get) {
    return(dat)
  } else {
    assign("payments", dat,envir = .GlobalEnv)
  }
}

### Update.Enrollments
Update.Enrollments <- function(get = FALSE, date = Sys.Date()) {
  fileName <- paste0("Enrolled Report  ", as.radiusDate(date), ".xlsx")
  filePath <- file.path(dataDir, fileName)
  
  if(!file.exists(filePath)) {
    #Try to move fileName from downloads
    moveDataDownloads(fileName)
    if(!file.exists(filePath)){
      stop(paste0("Up to date \n\"", filePath, "\"\nnot found"))
    }
  }
  #Implied else, file must exists
  dat <- read_excel(filePath, .name_repair = "unique_quiet")
  names(dat) <- gsub(" ", "_", names(dat))
  
  if (get) {
    return(dat)
  } else {
    assign("enrollments",dat,envir = .GlobalEnv)
  }
}#eof

### Update.Curriculum
Update.Curriculum <- function(get = FALSE, date = Sys.Date()){
  fileName <- paste0("Curriculum Library Export  ", as.radiusDate(date), ".xlsx")
  filePath <- file.path(dataDir, fileName)
  
  if(!file.exists(filePath)) {
    #Try to move fileName from downloads
    moveDataDownloads(fileName)
    if(!file.exists(filePath)){
      stop(paste0("Up to date \n\"", filePath, "\"\nnot found"))
    }
  }
  #Implied else, file must exists
  dat <- read_excel(filePath, .name_repair = "unique_quiet")
  names(dat) <- gsub(" ", "_", names(dat))
  
  if(get) {
    return(dat)
  } else {
    assign("curriculumDatabase",dat,envir = .GlobalEnv)
  }
  
}#eof

### Update.Attendance
Update.Attendance <- function(date = Sys.Date()) {
  fileName <- paste0("Student Attendance Report Export  ", as.radiusDate(date), ".xlsx")
  filePath <- file.path(dataDir, fileName)
  
  if(!file.exists(filePath)) {
    #Try to move fileName from downloads
    moveDataDownloads(fileName)
    if(!file.exists(filePath)){
      stop(paste0("Up to date \n\"", filePath, "\"\nnot found"))
    }
  }
  #Implied else, file must exists
  dat <- read_excel(filePath, .name_repair = "unique_quiet")
  names(dat) <- gsub(" ", "_", names(dat))
  
  logfile <- file.path(dataDir, "/studentAttendanceLog.csv")
  
  if(!file.exists(logfile)){
    stop(paste0("While running Update.Attendance ", logfile,
                " was not found"))
  }
  #Create user friendly names
  names(dat) <- gsub("Minutes", "min", #less to type
                     gsub("Hours", "hr",   #Ditto
                          gsub("[()]", "",     #Parentheses are weird sometimes
                               gsub(" ","_",names(dat))))) #Spaces make $ difficult
  if(!file.exists(fileLoc)) {
    stop(paste0("While running Update.Attendance ", fileLoc, 
                " was not found."))
  } #else implied by stop()
  
  if(grepl("xlsx$", fileLoc)) {
    newdat <- read_xlsx(fileLoc)
  } else if (grepl("csv$", fileLoc)) {
    newdat <- read.csv(fileLoc)
  } else {
    stop(paste0("While running Update.Attendance ", fileLoc, 
                "was not able to be read."))
  }
  
  ###
  ### NOT FINISHED
  ### NOT FINISHED
  ### NOT FINISHED
  ###
  
  
}#eof
