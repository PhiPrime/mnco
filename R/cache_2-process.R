processCacheData <- function(data, type) {
  type <- match.cacheType(type)
  key <- cacheKey(type)

  processedData <- getCenterData() %>%
    filter(.data[[key]] == data[[key]]) %>%
    dplyr::left_join(data, by = key) %>%
    {match.fun(paste0("processCacheData.", type))(.)}

  message("processCacheData(): cleanCacheData() called for ", type)
  #dataClean <- cleanCacheData(data, type)
  # if (!identical(data, dataClean)) {
  #   stop("Invalid input was given")
  # }
  # NEED TO FINISH

  return(processedData)
}

processCacheData.centerHistory <- function(data) {
  # PROCESSING GOES HERE

  return(data)
}

processCacheData.sessionLength <- function(data) {
  # PROCESSING GOES HERE

  return(data)
}

processCacheData.suppression <- function(data) {
  # PROCESSING GOES HERE
  data %>%
    mutate(
      # ADD PEST INTO PROGRESS TIDY
      Pest = .data$Skills_Mastered/.data$Attendances,
      creation = Sys.Date(),
    ) %>%
    select(
      "Student",
      "Skills_Currently_Assigned",
      "Pest",
      "Skills_Mastered",
      "Attendances",
      "creation",
      "expDate"
    ) %>%
    return()
}

processCacheData.template <- function(data) {
  # PROCESSING GOES HERE

  return(data)
}

processCacheData.vacation <- function(data) {
  data %>%
    # RENAME THIS IN TIDY
    dplyr::mutate(
      Last_Attendance = .data$Last_Attendance_Date
    ) %>%
    select(
      "Student",
      "Last_Attendance",
      "Return_Date"
    ) %>%
    return()
}

processCacheData.test <- function(data) {
  # PROCESSING GOES HERE

  return(data)
}
