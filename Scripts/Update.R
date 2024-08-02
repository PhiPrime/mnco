#############################     LIBRARIES     #############################
library(tidyverse)
library(lubridate)
library(readxl)

#########################     UPDATE FUNCTIONS     ##########################
### getCenterData
# Returns data frame containing center data for a given date
getCenterData <- function(date = Sys.Date(), ignoreMissing = F) {
  # Read and process raw data
  students <- getStudentData(date, ignoreMissing)
  accounts <- getAccountData(date, ignoreMissing)
  progress <- getProgressData(date, ignoreMissing)
  enrollments <- getEnrollmentData(date, ignoreMissing)
  
  # Merge into one data frame
  # NEED TO EXAMINE MERGING
  # MERGE getStudentRanking()
  all <- mergeWithFill(students, accounts, .by = "Account_Id")
  all <- merge(all, progress, all.x = TRUE)
  all <- merge(all, enrollments, all.x = TRUE)
  
  return(invisible(all))
}#eof

### getStudentData
# Returns data processed from Radius's "Students Export" file
getStudentData <- function(date = Sys.Date(), ignoreMissing = F){
  dat <- readRawData("Students Export", date, ignoreMissing)
  
  # Rename columns
  names(dat)[names(dat) == "Lead_Id...2"] <- "Lead_Id"
  
  # Reformat columns
  dat <- mutate(dat, Last_Attendance_Date = as.Date(Last_Attendance_Date, format = "%m/%d/%Y"))
  
  # Create new columns
  dat <- mutate(dat, Student = paste(First_Name, Last_Name), .before = Student_Id)
  
  # Columns to be removed
  # ORGANIZE rm_cols
  # COULD MAKE FLAG FOR CARD LEVEL UPGRADE
  rm_cols <- c("First_Name", "Last_Name", "School_Year", "Lead_Id...24", 
               "Created_By", "Stars_on_Current_Card", "Last_Modified_By",
               "Cards_Available", "Student_Notes", "Consent_to_Media_Release",
               "Consent_to_Contact_Teacher", "Consent_to_Leave_Unescorted",
               "Last_Modified_On", "Center_Id", "Virtual_Center",
               "Emergency_Contact", "Emergency_Phone")
  
  na_cols <- c("Billing_Street_1", "Billing_Street_2", "Billing_City",
               "Billing_State", "Billing_Country", "Billing_Zip_Code",
               "Scholarship", "School_[WebLead]", "Teacher_[WebLead]")
  # REXAMINE COLUMNS AFTER ABOVE ARE REVIEWED
  
  # Remove columns
  dat <- removeRawCols(dat, rm_cols)
  dat <- removeRawCols(dat, na_cols, test_na = T)
  
  return (dat)
}#eof

### getAccountData
# Returns data processed from Radius's "Account Export" file
getAccountData <- function(date = Sys.Date(), ignoreMissing = F){
  dat <- readRawData("Account Export", date, ignoreMissing)
  
  # Create new columns
  dat <- mutate(dat, Account = paste0(Last_Name, ", ", First_Name), .before = Account_Id)
  
  # Columns to be removed
  rm_cols <- c("First_Name", "Last_Name", "Last_Modified_By...20",
               "Last_Modified_By...33", "Description", "Customer_Comments",
               "Last_Modified_Date", "Emergency_Phone_Number", 
               "Emergency_Contact", "Account_Relation")
  na_cols <- c("Date_of_Birth", "Last_TriMathlon_Reg._Date")
  # REXAMINE COLUMNS AFTER ABOVE ARE REVIEWED
  
  # Remove columns
  dat <- removeRawCols(dat, rm_cols)
  dat <- removeRawCols(dat, na_cols, test_na = T)
  
  return(dat)
}#eof

### getProgressData
# Returns data processed from Radius's "Current Batch Detail Export" file
# Contains rolling 30 days info on attendances and skills
getProgressData <- function(date = Sys.Date(), ignoreMissing = F) {
  defaultRoot <-"Current Batch Detail Export"
  bootstrapRoot <- "Student Report"
  
  dat <- readRawData(defaultRoot, date, ignoreMissing = T)
  
  # FIGURE OUT HOW TO THROW AND CATCH ERROR PROPERLY
  if (nrow(dat) == 0) {
    tryCatch(
      {
        dat <-
          readRawData(bootstrapRoot, date) %>%
          
          # REFORMAT THESE COLUMNS
          mutate(
            Student = dat$Student_Name,
            Guardian = dat$Guardians,
            Account = dat$Account_Name,
            Active_Learning_Plans = dat$Active_LPs,
            Attendances = dat$Attendance,
            Skills_Mastered = dat$Skills_Mastered,
            Skills_Currently_Assigned = dat$Skills_Assigned,
            Enrollment_Status = dat$Enrollment_Status,
            BPR_Comment = NA,
            Last_PR_Send_Date = dat$Last_PR_Sent,
            Email_Opt_Out = NA,
          )
        
        # ADD CHECK FOR ATTENDANCES BEING TOO HIGH
        message(cat(
          "NOTICE: \"", as.rawFileName(defaultRoot, date), "\"\n",
          "\t\tis being used instead of\n", 
          "\t\"", as.rawFileName(bootstrapRoot, date), "\"\n",
          sep=""
        ))
      },
      
      error = function(e) {
        if (!ignoreMissing) {
          stop("Neither \"", as.rawFileName(defaultRoot, date), "\" or \"",
               as.rawFileName(bootstrapRoot, date), "\"\n\tfound in Downloads or ",
               "Raw_Data directories.")
        }
      }
    )
  }
  
  # PROCESS COLUMNS HERE
  rm_cols <- c("Guardian")
  na_cols <- c("BPR_Comment")
  
  maybe_cols <- c("Email_Opt_Out")
  
  # Remove columns
  dat <- removeRawCols(dat, rm_cols)
  dat <- removeRawCols(dat, na_cols, test_na = T)
  
  return(dat)
}#eof

getStudentRanking <- function(date = Sys.Date()) {
  # Get the relevant data
  progress <- getProgressData(date) %>%
    select(Student, Skills_Mastered, Attendances)
  
  deliveryKey <- getEnrollmentData(date) %>%
    select(Student, Monthly_Sessions, Delivery)
  
  differentDurationStudents <- 
    read.csv("Cache/differentDurationStudents.csv")
  
  # Merge and filter the data
  dat <- progress %>%
    merge(differentDurationStudents, all.x = T) %>%
    merge(deliveryKey, all.x = T) %>%
    
    # Scale attendances based on session length
    mutate(Duration = coalesce(Duration, 60),
           Attendances = Attendances * Duration / 60) %>% 
    
    # Subset valid contestants
    filter(Attendances >= Monthly_Sessions / 2,
           Skills_Mastered > 2,
           Delivery == "In-Center")
    
  #Create statistics for based on CI
  CI <- 95
  outlierThreshold <- 4
  roundingDig <- 4
  
  # Calculate ranking
  dat <- dat %>%
    mutate(
      Pest = Skills_Mastered / Attendances,
      
      #Outlier test
      zscore = (mean(Pest) - Pest) / (sd(Pest) / sqrt(Attendances)),
      samdev = sd(Pest[abs(zscore) < outlierThreshold]),
      
      UB = round(Pest - qnorm((1 - CI / 100) / 2) *
                   samdev / sqrt(Attendances), roundingDig),
      LB = round(Pest + qnorm((1 - CI / 100) / 2) *
                   samdev / sqrt(Attendances), roundingDig),
      
      Font_Size = round(32 * LB / max(LB), 1),
      
      Rank = rank(-LB, ties.method = "min"),
      Rank_Display = paste0(
        Rank,
        case_when(
          Rank %% 100 %in% 11:13 ~ "th",
          Rank %% 10 == 1 ~ "st",
          Rank %% 10 == 2 ~ "nd",
          Rank %% 10 == 3 ~ "rd",
          TRUE ~ "th"
        )
      )
    ) %>%
    select(-samdev)
  
  # Reorder columns and sort by rank
  col_order <- union(
    c("Rank", "Student", "LB", "Pest", "UB", "zscore", "Font_Size", "Rank_Display"),
    names(dat)
  )
  
  dat <- dat %>% 
    select(all_of(col_order)) %>%
    arrange(Rank)
  
  return(dat)
}#eof

### getEnrollmentData
getEnrollmentData <- function(date = Sys.Date(), ignoreMissing = F) {
  dat <- readRawData("Enrolled Report", date, ignoreMissing)
  
  # Rename columns
  names(dat)[names(dat) == "Account_Name"] <- "Account"
  names(dat)[names(dat) == "Total_Sessions"] <- "Monthly_Sessions"
  names(dat)[names(dat) == "Primary_Enrollment_End"] <- "Contract_End_Date"
  
  # Reformat columns
  dat <- dat %>%
    mutate(Membership_Type = 
             gsub("^\\* ", "", Membership_Type)) %>%
    mutate(Enrollment_Contract_Length = 
             gsub(" months?", "", Enrollment_Contract_Length)) %>%
    mutate(Enrollment_Length_of_Stay = 
             gsub(" months?", "", Enrollment_Length_of_Stay)) %>%
    mutate(Student_Length_of_Stay = 
             gsub(" months?", "", Student_Length_of_Stay)) %>%
  
    mutate(Monthly_Sessions = as.numeric(Monthly_Sessions)) %>%
    mutate(Delivery = as.factor(Delivery))
  
  # Create new columns
  dat <- mutate(dat, Student = paste(Student_First_Name, Student_Last_Name),
                .before = Student_First_Name)
  
  # Columns to be removed
  # Session_Length is handled as Duration in getProgressData()
  rm_cols <- c("Student_First_Name", "Student_Last_Name", "Session_Length",
               "Virtual_Center", "Status")
  na_cols <- c()
  # REXAMINE COLUMNS AFTER ABOVE ARE REVIEWED
  
  # Remove columns
  dat <- dat %>%
    removeRawCols(rm_cols)
    removeRawCols(na_cols, test_na = T)
  
  return(dat)
}#eof

### getPaymentData
getPaymentData <- function(date = Sys.Date(), ignoreMissing = F) {
  dat <- readRawData("Payments.xlsx", date)
  
  return(dat)
}#eof

### getCurriculumData
getCurriculumData <- function(date = Sys.Date(), ignoreMissing = F) {
  dat <- readRawData("Curriculum Library Export", date)
  
  return(dat)
}#eof

### getAttendanceHistory
getAttendanceTrainingSet <- function() {
  readRDS(file.path(getwd(), "Cache", "prior2024.rds"))
}#eof

### getAttendanceData
getAttendanceData <- function(get = FALSE, date = Sys.Date()) {
  dat <- readRawData("Student Attendance Report Export", date)
  
  logfile <- file.path(getwd(), "Cache", "studentAttendanceLog.csv")

  if(!file.exists(logfile)) {
    stop("While running getAttendanceData(), \"", logfile,
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

##saveTemplates 
## Uses "Template Export" with given date and checks if cache needs updated
saveTemplates <- function(date = Sys.Date()) {
  
  cacheFile <- file.path(getwd(), "/Cache/Templates.rds")
  newFile <- readRawData("Template Export", date) %>%
    #Mark LA timezone, as that's what Radius stores
    mutate(
      Last_Modified_Date = lubridate::force_tz(
        Last_Modified_Date, "America/Los_Angeles"),
      Created_Date = lubridate::force_tz(
        Created_Date, "America/Los_Angeles"),
      template = NA_character_)
  
  if(file.exists(cacheFile)){
    #If cache exists, pull it in and look for what's new
    cache <- readRDS(cacheFile)
    #tmp contains ID & cache's Modified Date
    tmp <- mutate(cache, Old_Date = Last_Modified_Date,
                  cachedTemplate = template) %>%
      select(Created_Date, Old_Date, cachedTemplate)
    
    #Updated is a boolean that is TRUE for positions in newFile that
    # need to be updated
    updated <- which(with(merge(newFile, tmp),
                          Last_Modified_Date!=Old_Date|
                            is.na(cachedTemplate)))
    
    #newLines are rows that need filled
    newLines <- newFile[updated,]
    newFile[!updated,] <- merge(select(newFile[!updated,], -template),
                                #If not updated use data in cache
                                cache)
  } else {#If no cache file everything will need updated
    newLines <- newFile
  }
  
  #Iterate through new rows and prompt user to input 
  # template body for each one
  for(i in as.numeric(rownames(newLines))) {
    #Each new one needs filled by user input
    ### NOTE: This does not record line breaks in email body
    message(paste0(
      "Enter the Body for the template named ",
      newFile[i,]$Template_Name, ":"))
    
    newFile[i,]$template <- paste0(scan(what = ""), sep = " ", collapse =)
  }
  
  saveRDS(newFile, cacheFile)
}

##getTemplate
## Uses Template cache to return template based on creation date
getTemplate  <- function(createdDate = "10/19/2021 6:55:40 PM") {
  idDate <- strptime(createdDate, "%m/%d/%Y %I:%M:%S %p")
  regexEx <- gsub(" [AP]M", "", 
                  gsub(" [0-9]+:", " [0-9]+:", idDate))
  
  cacheFile <- file.path(getwd(), "/Cache/Templates.rds")
  
  dat <- readRDS(cacheFile)
  rtn <- dat[grepl(regexEx, dat$Created_Date),]$template
  
}

#####################     UPDATE UTILITY FUNCTIONS     ######################

### mergeWithFill
# Merge columns from 'df2' into 'df1', matching rows based on common columns
#   in '.by'.
# Common columns not in '.by' are filled with values from 'df1'. If values 
#   are NA, they are filled from 'df2'.
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
}#eof

### removeRawCols
# Deletes columns from output of readRawData()
removeRawCols <- function(df, ..., test_na = F) {
  # Iterate through column names
  for (col_name in unlist(list(...))) {
    # Test columns
    if (!(col_name %in% names(df))) {
      # Column doesn't exist
      stop("Column \'", col_name, "\' does not exist in data frame.")
    } else if ( test_na && !all(is.na(df[[col_name]])) ) {
      # Column not NA but should be
      stop("Column \'", col_name, "\' is expected to be NA but isn't.",
           "\n  Run print_raw_na_cols() to get today's list of NA columns.")
    }
    
    # Delete column
    df[[col_name]] <- NULL
  }
  return(df)
}#eof

### readRawData
# Reads raw data excel file from Radius. If the file is in Downloads, it is
#   moved to Raw_Data directory.
readRawData <- function(fileRoot, date, 
                        ignoreMissing = F, regExFile = F) {
  # Format file root into Radius style file name
  fileName <- as.rawFileName(fileRoot, date)
  filePath <- NULL
  
  #If regex find a match with fileRoot in either folder
  #If regex find a match with fileRoot in either folder
  if(regExFile) {
    #Compare first to rawFiles,
    rawFiles <- list.files(file.path(getwd(), "Raw_Data"))
    fileOptions <- rawFiles[grepl(fileName, rawFiles)]
    
    if(length(fileOptions)==1){
      #If only one is found assign it
      fileName <- fileOptions[1]
    }else if(length(fileOptions)>1){ 
      #If more than 1, error
      stop(paste0("\"",fileName, "\" matched with the following files...", 
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
      fileOptions <- downloadFiles[grepl(fileName, downloadFiles)]
      
      
      if(length(fileOptions)==1){
        #If only one is found assign it
        fileName <- fileOptions
      }else if(length(fileOptions)>1){
        #If more than 1 match, error
        stop(paste0("\"",fileName, "\" matched with the following files...", 
                    paste("",fileOptions, sep = "\"\n\"", collapse = ""),
                    "\"\n...and does not know how to proceed, ",
                    "be more specific and try again."))
      } else if (length(fileOptions)==0){
        #If no matches stop and error
        stop(paste0("After searching both \"./Raw_Data\",",
                    "and \"./Downloads\" \"", fileName, "\" yielded no matches.",
                    " Please try again."))
      }}
  }
  # Default behavior: attempt to move file from Downloads, then look
  #   in Raw_Data
  filePath <- file.path(getwd(), "Raw_Data", fileName)
  fileMoved <- moveDataDownloads(fileName)
  
  if (!fileMoved && !file.exists(filePath)) {
    # File not found in Downloads or Raw_Data
    if (!ignoreMissing) {
      stop("\"", fileName, "\" not found in Raw_Data or Downloads directories")
    } else {
      # Set filePath to read empty version of the raw data file
      emptyFileName <- paste0(fileRoot, "  EMPTY", ".xlsx")
      filePath <- file.path(getwd(), "Raw_Helper", emptyFileName)
    }
  }
  
  
  # Read file and reformat column names to prevent bad behaviors
  dat <- read_excel(filePath, .name_repair = "unique_quiet")
  names(dat) <- gsub(" ", "_", names(dat))
  
  return(dat)
}#eof

### as.rawFileName
# Formats raw data file root as Radius style file name
as.rawFileName <- function(file_root, date = Sys.Date()){
  paste0(file_root, "  ", paste(month(date), day(date), year(date), sep = "_"), 
         ".xlsx")
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
    # CHANGE TO NOT PRINT FULL PATH
    message(cat("NOTICE: ", file_path, 
                "\n\t\t-- moved to -->\n\t", file_dest, "\n", sep=""))
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
}#eof

removeDifferentDurationStudent <- function(student) {
  # need to add file check
  filePath <- file.path(getwd(), "Cache/differentDurationStudents.csv")
  dat <- read.csv(filePath)
  
  dat <- dat[dat$Student != student,]
  write.csv(dat, filePath, row.names = F)
}#eof

templatesNeedUpdated <- function(date = Sys.Date()) {
  cacheFile <- file.path(getwd(), "/Cache/Templates.rds")
  newFile <- readRawData("Template Export", date) %>%
    #Mark LA timezone, as that's what Radius stores
    mutate(
      Last_Modified_Date = lubridate::force_tz(
        Last_Modified_Date, "America/Los_Angeles"),
      Created_Date = lubridate::force_tz(
        Created_Date, "America/Los_Angeles"),
      template = NA_character_)
  
    #Tmp contains ID & Current modified Date
    tmp <- mutate(newFile, Current_Date = Last_Modified_Date) %>%
      select(Created_Date, Current_Date)
    #If any dates are different from cache return TRUE
    rtn <- any(with(merge(readRDS(cacheFile), tmp),
                    Last_Modified_Date!=Current_Date))
    return(rtn)
  }#eof

#For creating format of text files using an id & student name
asMessageTxtFile <- function(id, student){
  return(paste0(gsub("[ -]", "_", 
              paste(id,student, sep = "__")),".txt"))
}
