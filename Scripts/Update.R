### Libraries
library(tidyverse)
library(lubridate)
library(readxl)

#### Update functions:

### getCenterData
getCenterData <- function(date = Sys.Date(), ignoreMissing = F) {
  # Read and process excel files
  students <- getStudentData(date, ignoreMissing)
  accounts <- Update.Accounts(TRUE, date, ignoreMissing)
  progress <- Update.Progress(TRUE, date, ignoreMissing)
  enrollments <- Update.Enrollments(TRUE, date, ignoreMissing)
  
  # Merge into one data frame
  all <- mergeWithFill(students, accounts, .by = "Account_Id")
  all <- merge(all, progress, all.x = TRUE)
  all <- merge(all, enrollments, all.x = TRUE)
  
  return(all)
}

### mergeWithFill
# Merge columns from 'df2' into 'df1', matching observations based on 
#   the specified columns.
# Common columns not in '.by' are filled with values from 'df1'. 
#   If values are NA, they are filled from 'df2'.
mergeWithFill <- function(df1, df2, .by) {
  # Merge df1 and df2. Common columns are suffixed with .x and .y
  df <- merge(df1, df2, all.x = T, by = .by)
  
  # Iterate through common columns
  for (colName in intersect(names(df1), names(df2))) {
    # Skip iteration if column was used to match
    if (colName %in% .by) next
    
    # Suffixed column strings
    colName.x <- paste0(colName, ".x")
    colName.y <- paste0(colName, ".y")
    
    # Use NA if column is empty
    # DOES NOT WORK ON EMPTY DATA FRAMES YET
    # NEED TO PROPERLY MERGE DATA IN getCenterData() THEN FIND SOLUTION
    #col.x <- ifelse(!identical(df[[colName.x]], logical(0)), df[[colName.x]], NA)
    #col.y <- ifelse(!identical(df[[colName.y]], logical(0)), df[[colName.y]], NA)
    #col.x <- df[[colName.x]]
    #col.y <- df[[colName.y]]
    
    # Fill value for common column to col.x and rename to col
    df[[colName.x]] <- coalesce(df[[colName.x]], df[[colName.y]])
    names(df)[names(df) == colName.x] <- colName
    # CHANGE TO THIS? MAYBE DOESN'T WORK
    #df <- rename(df, col = col.x)
    
    # Delete col.y
    df[[colName.y]] <- NULL
  }
  
  return(df)
}

### remove_raw_cols
# Deletes columns from output of Update.Init()
# test parameter determines ???
remove_raw_cols <- function(df, ..., test_na = F) {
  for (col_name in unlist(list(...))) {
    # Test columns for conditions based on test argument
    if (!(col_name %in% names(df))) {
      stop("Column \'", col_name, "\' does not exist in data frame.")
    } else if ( test_na && !all(is.na(df[[col_name]])) ) {
      stop("Column \'", col_name, "\' is expected to be NA but isn't.",
           "\n  Run print_raw_na_cols() to get today's list of NA columns.")
    }
    
    df[[col_name]] <- NULL
  }
  return(df)
}

### Update.Init
Update.Init <- function(fileRoot, date, ignoreMissing = F, regExFile = FALSE) {
  fileName <- as.rawFileName(fileRoot, date)
  
  #If regex find a match with fileRoot in either folder
  if(regExFile) {
    #Compare first to rawFiles,
    rawFiles <- list.files(file.path(getwd(), "Raw_Data"))
    fileOptions <- rawFiles[grepl(fileRoot, rawFiles)]
    
    if(length(fileOptions)==1){
      #If only one is found assign it
      filePath <- fileOptions
    }else if(length(fileOptions)>1){ 
      #If more than 1, error
      stop(paste0("\"",fileRoot, "\" matched with the following files...", 
                  paste("",fileOptions, sep = "\"\n\"", collapse = ""),
                  "\"\n...and does not know how to proceed, ",
                  "be more specific and try again."))
    } else if (length(fileOptions)==0){
      #If not found compare to downloads folder
      downloadPath <- file.path(regmatches(getwd(), 
                                           regexpr("^.*?[/].*?[/].*?(?=/)",
                                                   getwd(), perl = T)), 
                                "Downloads")
      downloadFiles <- list.files(downloadPath)
      fileOptions <- downloadFiles[grepl(fileRoot, downloadFiles)]
      
      
      if(length(fileOptions)==1){
        #If only one is found assign it
        filePath <- fileOptions
      }else if(length(fileOptions)>1){
        #If more than 1 match, error
        stop(paste0("\"",fileRoot, "\" matched with the following files...", 
                    paste("",fileOptions, sep = "\"\n\"", collapse = ""),
                    "\"\n...and does not know how to proceed, ",
                    "be more specific and try again."))
      } else if (length(fileOptions)==0){
        #If no matches stop and error
        stop(paste0("After searching both \"./Raw_Data\",",
                    "and \"./Downloads\" \"", fileRoot, "\" yielded no matches.",
                    " Please try again."))
      }
    }
    dat <- read_excel(filePath, .name_repair = "unique_quiet")
  } else {
    # Default behavior: not regex and filePath should be set or moved 
    filePath <- file.path(getwd(), "Raw_Data", fileName)
    
    fileMoved <- moveDataDownloads(fileName)
    
    if (!fileMoved && !file.exists(filePath)) {
      if (!ignoreMissing) {
        stop("\"", fileName, "\" not found in Raw_Data/ or Downloads/")
      } else {
        emptyFileName <- paste0(fileRoot, "EMPTY", ".xlsx")
        emptyFilePath <- file.path(getwd(), "Raw_Helper", emptyFileName)
        
        dat <- read_excel(emptyFilePath, .name_repair = "unique_quiet")
      }
    } else {
      dat <- read_excel(filePath, .name_repair = "unique_quiet")
    }
  }
  
  names(dat) <- gsub(" ", "_", names(dat))
  return(dat)
}#eof

as.dataFilePath <- function(fileName, date = Sys.Date()){
  #I made one big line  
  return(
    file.path(paste0(getwd(),"/Raw_Data"), 
              paste0(fileName, 
                     paste(lubridate::month(date), 
                           lubridate::day(date), 
                           lubridate::year(date), sep = "_"), 
                     ".xlsx")))
}

as.rawFileName <- function(file_root, date = Sys.Date()){
  paste0(file_root, paste(month(date), day(date), year(date), sep = "_"), 
         ".xlsx")
}

### getStudentData
getStudentData <- function(date = Sys.Date(), ignoreMissing = F){
  # Read data from excel file
  dat <- Update.Init("Students Export  ", date, ignoreMissing)
  
  # Rename columns
  names(dat)[names(dat) == "Lead_Id...2"] <- "Lead_Id"
  
  # Reformat columns
  dat <- mutate(dat, Last_Attendance_Date = as.Date(Last_Attendance_Date, format = "%m/%d/%Y"))
  
  # Create columns from other columns
  dat <- mutate(dat, Student = paste(First_Name, Last_Name), .before = Student_Id)
  
  # Columns to be removed
  rm_cols <- c("First_Name", "Last_Name", "School_Year", "Lead_Id...24", 
               "Created_By", "Card_Level", "Stars_on_Current_Card",
               "Cards_Available")
  na_cols <- c("Billing_Street_1", "Billing_Street_2", "Billing_City",
               "Billing_State", "Billing_Country", "Billing_Zip_Code",
               "Scholarship", "School_[WebLead]", "Teacher_[WebLead]")
  
  # maybe needed?
  maybe_cols <- c("Enrollment_Start_Date", "Enrollment_End_Date", "Last_PR_Sent",
                  "Description", "Student_Notes")
  maybe_cols2 <- c("Last_Attendance_Date", "Last_PR_Date")
  # not needed?
  maybe_cols3 <- c("Consent_to_Media_Release",
                   "Consent_to_Contact_Teacher", "Consent_to_Leave_Unescorted")
  maybe_cols4 <- c("Emergency_Contact", "Emergency_Phone", "Medical_Information")
  maybe_cols5 <- c("Created_Date", "Last_Modified_On")
  # all the same
  maybe_cols6 <- c("Center_Id", "Center", "Virtual_Center")
  # REXAMINE COLUMNS AFTER ABOVE ARE REVIEWED
  
  # Remove columns
  dat <- remove_raw_cols(dat, rm_cols)
  dat <- remove_raw_cols(dat, na_cols, test_na = T)
  
  return (dat)
}#eof

### Update.Accounts
Update.Accounts <- function(get = FALSE, date = Sys.Date(), ignoreMissing = F){
  #Update Initialize
  dat <- Update.Init("Account Export  ", date, ignoreMissing)
  
  # Create columns from other columns
  dat <- mutate(dat, Account = paste0(Last_Name, ", ", First_Name), .before = Account_Id)
  
  # Columns to be removed
  rm_cols <- c("First_Name", "Last_Name", "Created_By", "Last_Modified_By...20",
               "Last_Modified_By...33")
  na_cols <- c("Date_of_Birth", "Last_TriMathlon_Reg._Date")
  
  # not needed?
  maybe_cols = c("Center", "Description", "Customer_Comments",
                 "Referral_Accounts", "Account_Relation",
                 "Last_Modified_Date", "Created_Date")
  # REXAMINE COLUMNS AFTER ABOVE ARE REVIEWED
  
  # Remove columns
  dat <- remove_raw_cols(dat, rm_cols)
  dat <- remove_raw_cols(dat, na_cols, test_na = T)
  
  if(get) {
    return(dat)
  } else {
    accounts <- filter(dat, Enrollment_Status == "Active")
    inactive <- filter(dat, Enrollment_Status == "Inactive")
    
    assign("accounts",accounts,envir = .GlobalEnv)
    assign("inactiveAccounts",inactive,envir = .GlobalEnv)
  }
}#eof

### Update.Progress
Update.Progress <- function(get = FALSE, date = Sys.Date(), ignoreMissing = F) {
  # Update Initialize
  fileRoot <- "Current Batch Detail Export  "
  filePath <- file.path(getwd(), "Raw_Data", as.rawFileName(fileRoot))
  
  # NEED TO REORGANIZE THIS SOMEHOW
  if(!file.exists(filePath)){
    #Try to move from downloads
    moveDataDownloads(gsub(".*/", "", filePath))
    if(!file.exists(filePath)){
      #Check for "bootstrap" files
      fileRoot2 <- "Student Report  "
      dat <- Update.Init(fileRoot2, date, ignoreMissing)
        
      cat("Notice: ", file.path(getwd(), "Raw_Data", as.rawFileName(fileRoot2)), 
                   "\n\t\tis being used instead of\n\t", filePath, sep="")
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
  
  if(file.exists(filePath)) {
    dat <- Update.Init(fileRoot, date, ignoreMissing)
  }
  
  
  #merge qualifying students from tdat back into dat
  ###
  
  
  if(get) {
    return(dat)
  } else {
    tdat <- getStudentRanking(date)
    assign("studentProgress",dat,envir = .GlobalEnv)
    assign("selectStudentProgress", tdat, envir = .GlobalEnv)
    assign("studentRanking", select(tdat, 
                                    Rank, Student, fontsize, UB, Pest, LB, 
                                    Skills_Mastered, Attendances), envir = .GlobalEnv)
  }
}#eof

getStudentRanking <- function(date = Sys.Date()){
  dat <- Update.Progress(TRUE, date)
  
  #Merge in delivery record from Enrollments
  deliveryKey <- mutate(Update.Enrollments(TRUE, date), 
                        Student = paste(Student_First_Name, 
                                        Student_Last_Name),
                        Delivery = as.factor(Delivery),
                        Monthly_Sessions = as.numeric(Total_Sessions)) %>%
    select(Student, Delivery, Monthly_Sessions)
  
  dat <- merge(dat,deliveryKey)
  
  #Sort and select desired data
  # Add column for session duration
  differentDurationStudents <- read.csv("Cache/differentDurationStudents.csv")
  dat <- merge(dat, differentDurationStudents, all.x = T)
  dat$Duration <- coalesce(dat$Duration, 60)
  dat <- mutate(dat, Attendances = Attendances * Duration/60)
  
  #Subset valid contestants
  dat <- dat[which(dat$Attendances >= dat$Monthly_Sessions/2 & 
                     dat$Skills_Mastered>2 & 
                     dat$Delivery == "In-Center"),]
  
  #Create statistics for based on CI
  CI <- 95
  outlierThreshold <- 4
  roundingDig <- 4
  dat <- mutate(dat, Pest = Skills_Mastered/Attendances)
  
  #Outlier test
  dat <- mutate(dat, 
                zscore = (mean(dat$Pest)-Pest)/
                  (sd(dat$Pest)/sqrt(Attendances)))
  #Get sd without outliers
  samdev <- sd(dat$Pest[dat$zscore<outlierThreshold])
  dat <- mutate(dat, 
                UB = round(Pest-qnorm((1-CI/100)/2)*
                             samdev/sqrt(Attendances),roundingDig),
                LB = round(Pest+qnorm((1-CI/100)/2)*
                             samdev/sqrt(Attendances),roundingDig))
  dat <- mutate(dat, fontsize = round(32*LB/max(dat$LB), 1))
  dat <- dat[order(dat$LB, decreasing = TRUE),]
  dat <- mutate(dat, Rank = dim(dat)[1] +1 - rank(LB, ties.method = "max"))
  
  
  dat <- mutate(dat, rankSuffix = ifelse(grepl("[2-9]?1$", 
                                               as.character(Rank)), "st",
                                         ifelse(grepl("[2-9]?2$",
                                                      as.character(Rank)), "nd",
                                                ifelse(grepl("[2-9]?3$",
                                                             as.character(Rank)),
                                                       "rd","th")))) %>%
    mutate(rankDisplay = paste0(Rank, rankSuffix))
  
  #Select reasonable data
  # dat <- select(dat, )
  
  return(dat)
}#eof

### Update.Enrollments
Update.Enrollments <- function(get = FALSE, date = Sys.Date(), ignoreMissing = F) {
  #Update Initialize
  dat <- Update.Init("Enrolled Report  ", date, ignoreMissing)
  
  # Rename columns
  names(dat)[names(dat) == "Account_Name"] <- "Account"
  
  # Reformat columns
  # NEED TO REFORMAT Membership_Type
  
  # Create columns from other columns
  dat <- mutate(dat, Student = paste(Student_First_Name, Student_Last_Name),
                .before = Student_First_Name)
  
  # Columns to be removed
  # Session_Length is handled as Duration in Update.Progress()
  rm_cols <- c("Student_First_Name", "Student_Last_Name", "Session_Length")
  na_cols <- c()
  
  # all the same
  maybe_cols <- c("Center", "Virtual_Center", "Status")
  # maybe needed?
  maybe_cols2 <- c("Primary_Enrollment_Start", "Primary_Enrollment_End", 
                   "Expected_Monthly_Amount")
  # REXAMINE COLUMNS AFTER ABOVE ARE REVIEWED
  
  # Remove columns
  dat <- remove_raw_cols(dat, rm_cols)
  dat <- remove_raw_cols(dat, na_cols, test_na = T)
  
  if (get) {
    return(dat)
  } else {
    assign("enrollments",dat,envir = .GlobalEnv)
  }
}#eof

### Update.Curriculum
Update.Curriculum <- function(get = FALSE, date = Sys.Date()){
  #Update Initialize
  dat <- Update.Init("Curriculum Library Export  ", date)
  
  if(get) {
    return(dat)
  } else {
    assign("curriculumDatabase",dat,envir = .GlobalEnv)
  }
  
}#eof

getHistoricAttendance <- function(){
  return(readRDS(file.path(paste0(getwd(),
                                   "/Cache"), "prior2024.rds")))
}

### Update.Attendance
Update.Attendance <- function(get = FALSE, date = Sys.Date()) {
  #Update Initialize
  dat <- Update.Init("Student Attendance Report Export  ", date)
  
  logfile <- file.path(getwd(), "Cache", "studentAttendanceLog.csv")

  if(!file.exists(logfile)) {
    stop("While running Update.Attendance(), \"", logfile,
                "\" was not found.")
  } else {
    logdat <- read.csv(logfile)
  }
  
    #Mutate to tidy
    dat <- mutate(dat,
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
    
    dat <- mutate(dat,
                  line = paste(accountID,date,name, 
                               sep = ";"))
    
    dat <- select(dat, date:delivery, line)
  
  
  newdat <- dat[!(dat$line %in% logdat$line),]
  
  #Check for and notify if no new data is found
  if(dim(newdat)[1]==0){
    cat("Notice: Update.Attendance(get = ", get, ", date = ", as.character(date),
      ") found no new attendance when updating.", sep = "")
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
  #   stop(paste0("While running Update.Attendance ", fileLoc, 
  #               "was not able to be read."))
  # }
  
  
}#eof

getAssessments <- function(updateGlobal = FALSE, 
                           date = Sys.Date(), ignoreMissing = F){
  #Update Initialize
  dat <- Update.Init("Students Export  ", date, ignoreMissing, regExFile = TRUE)
  
  dat <- mutate(dat, 
                Last_Attendance_Date = as.Date(Last_Attendance_Date, 
                                               format = "%m/%d/%Y"))
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




moveDataDownloads <- function(file_name) {
  download_path <- file.path(regmatches(getwd(), regexpr("^.*?[/].*?[/].*?(?=/)", 
                                          getwd(), perl = T)), "Downloads")
  file_path <- file.path(download_path, file_name)
  
  if (!file.exists(download_path)) {
    stop("Downloads folder not found at \"", download_path, "\"")
  }
  
  if(!grepl("Overview$", getwd())) {
    stop("While trying to moveDataDownloads, \"",
         getwd(),
         "\"\n\tis the working directory but does not end with \"Overview\"")
  }
  
  file_dest <- file.path(getwd(), "Raw_Data", file_name)
  file_moved <- F
  
  if (file.exists(file_path)) {
    file.rename(file_path, file_dest)
    cat("Notice: ", file_path, "\n\t\t-- moved to -->\n\t", file_dest, "\n", sep="")
    file_moved <- T
  }
  
  return(file_moved)
  
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
