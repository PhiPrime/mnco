settings_file <- "centerOverviewSettings.dat"

#' Create Settings File
#'
#'
#' @return name of file
#' @export
get_file_name <- function() {
  return(settings_file)
}

#' Initializes Settings File
#'
#' @export
initialize_settings_file <- function() {

  #Establish a connection
  file_connection <- file(settings_file)

  #set up the data
  write(c("Recent_Assessments_Date=8_5_2024",
          "Attendance_Allowed_Days=5",
          "Deck_Minimum_Threshold=5",
          "Deck_Warning_Duration=2",
          "Deck_Suppression_Maximum_Time=30",
          "Standard_Contract_Length=7",
          "Standard_Number_Sessions=10",
          "Price_Upper_Bound=368",
          "Price_Lower_Bound=275",
          "Student_Upper_Bound=100",
          "Student_Lower_Bound=50",
          "Length_Modifier=20",
          "Contract_Adjustment_Lower_Bound=60",
          "Contract_Adjustment_Upper_Bound=-10",
          "Contract_Length_Upper_Bound=12",
          "Contract_Length_Lower_Bound=1",
          "Raw_Data_Directory=UNINITIALIZED",
          "Cache_Directory=UNINITIALIZED"
          ),
        settings_file, append = FALSE)

  #close the connection
  close(file_connection)
}

#' Retrieve a variable from the settings file
#'
#' @param variable_name Name of the variable
#' @param is_numeric Boolean to determine how the variable is interpreted
#'
#' @return Value from the file
#' @export
retrieve_variable <- function(variable_name, is_numeric = TRUE) {

  #Create default if no settings file exists
  if (!file.exists(get_file_name()))
    initialize_settings_file()

  #Adjust parameters
  variable_name <- paste(variable_name, "=", sep="")
  value <- NA

  #Establish connection and read the lines
  file_connection <- file(settings_file)
  text_output  <- readLines(settings_file)

  #Brute force to find vector and retrieve the value
  for (i in 1:length(text_output)) {
    if (grepl(variable_name, text_output[i])) {
      value <- gsub(variable_name, "",text_output[i])
    }
  }

  #Close connection and return the value
  close(file_connection)
  if (!is_numeric)
    return(value)
  else
    return(strtoi(value))
}

#' Edit a variable in the settings file
#'
#' @param variable_name Name of the variable
#' @param value Value being written to the variable
#'
#' @export
edit_variable <- function(variable_name, value) {

  #Establish connection and read lines
  file_connection <- file(settings_file)
  text_output  <- readLines(settings_file)

  #Brute force search for variable
  for (i in 1:length(text_output)) {
    if (grepl(variable_name, text_output[i])) {
      #Separate the variable name and value before
      no_name_text <- gsub(variable_name, "",text_output[i])
      text_output[i] <- gsub(no_name_text, paste("=",value,sep=""), x = text_output[i])
      writeLines(text_output, con=settings_file)
    }
  }
  #Close connection
  close(file_connection)
}
