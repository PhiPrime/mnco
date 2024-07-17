### Libraries
library(tidyverse)
library(lubridate)
library(readxl)

#### Update functions:

### Update.All
Update.All <- function(get = FALSE){
  # Read excel files
  students <- Update.Students(TRUE)
  accounts <- Update.Accounts(TRUE)
  progress <- Update.Progress(TRUE)
  #payments <- Update.Payments(TRUE)
  #payments <- mutate(payments, Account = Account_Name)
  enrollments <- Update.Enrollments(TRUE)
  
  # Prepare students to merge
  students <- mutate(students, Student = paste(First_Name, Last_Name), .before = Student_Id)
  students$First_Name <- NULL
  students$Last_Name <- NULL
  
  # Prepare accounts to merge
  accounts$First_Name <- NULL
  accounts$Last_Name <- NULL
  
  # Merge into one data frame
  all <- mergeWithFill(students, accounts, .by = "Account_Id")
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

### mergeWithFill
# Merge columns from 'df2' into 'df1', matching observations based on the specified columns.
# Common columns not in '.by' are filled with values from 'df1'. If values are NA, they are filled from 'df2'.
mergeWithFill <- function(df1, df2, .by) {
  # Merge df1 and df2. Common columns are suffixed with .x and .y
  df <- merge(df1, df2, all.x = T, by = .by)
  
  # Iterate through common columns
  for (col in intersect(names(df1), names(df2))) {
    # Skip iteration if column was used to match
    if (col %in% .by) next
    
    # Suffixed column strings
    col.x <- paste0(col, ".x")
    col.y <- paste0(col, ".y")
    
    # Fill value for common column to col.x and rename to col
    df[[col.x]] <- coalesce(df[[col.x]], df[[col.y]])
    names(df)[names(df) == col.x] <- col
    
    # Delete col.y
    df[[col.y]] <- NULL
  }
  
  return(df)
}

### Update.Init
Update.Init <- function(fileRoot, date = Sys.Date()) {
  #set file name
  fileName <- paste0(fileRoot, 
                     paste(month(date), day(date), year(date), sep = "_"), 
                     ".xlsx")
  filePath <- file.path(getwd(), "Raw_Data", fileName)
  
  if(!file.exists(filePath)) {
    #Try to move fileName from downloads
    moveDataDownloads(fileName)
    
    if(!file.exists(filePath)) {
      stop(paste0("Up to date \n\"", filePath, "\"\nnot found"))
    }
  }
  
  #Implied else, file must exists
  dat <- read_excel(filePath, .name_repair = "unique_quiet")
  names(dat) <- gsub(" ", "_", names(dat))
  
  return(dat)
}#eof

as.dataFilePath <- function(fileName, date = Sys.Date()){
  #I made one big line  
  return(
    file.path(paste0(getwd(),"/Raw_Data"), 
              paste0(fileName, 
                     paste(month(date), day(date), year(date), sep = "_"), 
                     ".xlsx")))
}

### Update.Students
Update.Students <- function(get = FALSE, date = Sys.Date()){
  #Update Initialize
  dat <- Update.Init("Students Export  ")
  
  dat <- mutate(dat, 
                Last_Attendance_Date = as.Date(Last_Attendance_Date, 
                                               format = "%m/%d/%y"))
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

### Update.Accounts
Update.Accounts <- function(get = FALSE, date = Sys.Date()){
  #Update Initialize
  dat <- Update.Init("Account Export  ")
  
  accounts <- filter(dat, Enrollment_Status == "Active")
  inactive <- filter(dat, Enrollment_Status == "Inactive")
  
  if(get) {
    return(dat)
  } else {
    assign("accounts",accounts,envir = .GlobalEnv)
    assign("inactiveAccounts",inactive,envir = .GlobalEnv)
  }
}#eof

### Update.Progress
Update.Progress <- function(get = FALSE, date = Sys.Date()) {
  #Update Initialize
  fileRoot <- "Current Batch Detail Export  "
  filePath <- as.dataFilePath(fileRoot)
  
  if(!file.exists(filePath)){
    #Try to move from downloads
    moveDataDownloads(gsub(".*/", "", filePath))
    if(!file.exists(filePath)){
      #Check for "bootstrap" files
      fileRoot2 <- "Student Report  "
      dat <- Update.Init(fileRoot2)
        
      print(paste0("Notice: ", as.dataFilePath(fileRoot2), 
                   " is being used instead of ", filePath))
      dat <- mutate(dat,
             Student = dat$Student_Name,
             Guardian = dat$Guardians,
             Account = dat$Account_Name, #Unsure of formatting
             Active_Learning_Plans = dat$Active_LPs,
             Attendances = dat$Attendance,
             Skills_Mastered = dat$Skills_Mastered,
             Skills_Currently_Assigned = dat$Skills_Assigned,
             Enrollment_Status = dat$Enrollment_Status,
             BPR_Comment = NA,
             Last_PR_Send_Date = dat$Last_PR_Sent,
             Email_Opt_Out = NA,
      )
      }
    }#filePath should exist
  
  if(file.exists(filePath)){
    dat <- Update.Init(fileRoot)
    }
  
  tdat <- dat[which(!dat$Attendances<5 & !dat$Skills_Mastered<2),]
  
  #A specific student attends for 90 mins instead of 60

  
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
  #Update Initialize
  dat <- Update.Init("Payments.xlsx  ")
  
  if(get) {
    return(dat)
  } else {
    assign("payments", dat,envir = .GlobalEnv)
  }
}

### Update.Enrollments
Update.Enrollments <- function(get = FALSE, date = Sys.Date()) {
  #Update Initialize
  dat <- Update.Init("Enrolled Report  ")
  
  if (get) {
    return(dat)
  } else {
    assign("enrollments",dat,envir = .GlobalEnv)
  }
}#eof

### Update.Curriculum
Update.Curriculum <- function(get = FALSE, date = Sys.Date()){
  #Update Initialize
  dat <- Update.Init("Curriculum Library Export  ")
  
  if(get) {
    return(dat)
  } else {
    assign("curriculumDatabase",dat,envir = .GlobalEnv)
  }
  
}#eof

### Update.Attendance
Update.Attendance <- function(date = Sys.Date()) {
  #Update Initialize
  dat <- Update.Init("Student Attendance Report Export  ")
  
  logfile <- file.path(paste0(getwd(),"/Cache"), "studentAttendanceLog.csv")
  
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

moveDataDownloads <- function(fileNames) {
  rmPath <- gsub("^.*[/].*[/].*[/].*?", "", getwd())
  downloadPath <- paste0(gsub(rmPath, "", getwd()), "Downloads/")
  filePaths <- paste0(downloadPath, fileNames)
  
  if(!grepl("Overview$", getwd())) {
    stop("while trying to moveDataDownloads,\n",
         getwd(), "\nis the working directory but does not\n",
         "end with \"Overview\"")
  }
  
  fileDests <- paste0(getwd(), "/Raw_Data/", fileNames)
  
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

# Put this into a function?
# write.csv(data.frame(Student = character(), Duration = numeric()), "Cache/differentDurationStudents.csv", row.names = F)

addDifferentDurationStudent <- function(student, duration) {
  # need to add file check
  filePath <- file.path(getwd(), "Cache/differentDurationStudents.csv")
  dat <- read.csv(filePath)
  
  dat <- rbind(dat, data.frame(Student = student, Duration = duration))
  write.csv(dat, filePath, row.names = F)
}

removeDifferentDurationStudent <- function(student) {
  # need to add file check
  filePath <- file.path(getwd(), "Cache/differentDurationStudents.csv")
  dat <- read.csv(filePath)
  
  dat <- dat[dat$Student != student,]
  write.csv(dat, filePath, row.names = F)
}
