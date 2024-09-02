updateCache <- function(input, type, remove = FALSE) {
  type <- match.cacheType(type)

  path <- cachePath(type)
  cache <- readRDS(path)
  rowsAction <- if (!remove) dplyr::rows_upsert else dplyr::rows_delete

  return(match.fun(paste0("updateCache.", type))(input, cache, rowsAction))

  updated <-
    match.fun(paste0("updateCache.", type))(input, cache, rowsAction) %>%
    saveCache(type)

  return(updated)
}

filterJoin <- function(x, y, key) {
  filter(x, x[[key]] == y[[key]]) %>%
    patchJoin(y, .by = key, update = T) %>%
    return()
}

updateCache.centerHistory <- function(data) {
  # PROCESSING GOES HERE

  return(data)
}

updateCache.sessionLength <- function(data) {
  # PROCESSING GOES HERE

  return(data)
}

updateCache.suppression <- function(input, cache, rowsAction) {
  key <- "Student"
  data <- getCenterData()

  output <- data %>%
    filterJoin(input, key) %>%
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
    )

  output <- processedData %>%
    {tryCatch(
      rowsAction(cache, ., by = key),
      error = function(e) cache
    )} %>%
    filter(Sys.Date() <= .data$expDate)

  return(data)
}

updateCache.template <- function(data) {
  # PROCESSING GOES HERE

  return(data)
}

updateCache.vacation <- function(input, cache, rowsAction) {
  key <- "Student"
  data <- getCenterData()

  joinedData <- data %>% filterJoin(input, key)
  transformedData <- joinedData %>%
    # RENAME THIS IN TIDY
    dplyr::mutate(
      Last_Attendance = .data$Last_Attendance_Date
    ) %>%
    select(
      "Student",
      "Last_Attendance",
      "returnDate"
    )
  modifiedCache <- transformedData %>%
    {tryCatch(
      rowsAction(cache, ., by = key),
      error = function(e) cache
    )}

  cleanedCache <- modifiedCache %>%
    filter(Sys.Date() <= .data$returnDate) %>%
    dplyr::left_join(by = "Student",
      transmute(data,
        Student = data$Student,
        last_att_temp = data$Last_Attendance_Date
      )
    ) %>%
    filter(.data$last_att_temp <= .data$Last_Attendance) %>%
    select(-"last_att_temp")

  return(cleanedCache)
}

updateCache.test <- function(data) {
  # PROCESSING GOES HERE

  return(data)
}
