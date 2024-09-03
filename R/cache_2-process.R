updateCache <- function(input, type, action = c("upsert", "delete"), unmatched) {
  type <- match.cacheType(type)
  action <- match.arg(action)

  cache <- readRDS(cachePath(type))

  updated <-
    match.fun(paste0("updateCache.", type))(input, cache, action) %>%
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

updateCache.suppression <- function(input, cache, action) {
  key <- "Student"
  data <- getCenterData()

  if (action == "upsert") {
    joinedData <- data %>% filterJoin(input, key)

    transformedData <- joinedData %>%
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

    modifiedCache <-
      dplyr::rows_upsert(cache, transformedData, by = key)
  } else if (action == "delete") {
    modifiedCache <-
      dplyr::rows_delete(cache, input, by = key, unmatched = "ignore")
  }

  cleanedCache <- modifiedCache %>%
    filter(Sys.Date() <= .data$expDate)

  return(cleanedCache)
}

updateCache.template <- function(data) {
  # PROCESSING GOES HERE

  return(data)
}

updateCache.vacation <- function(input, cache, action) {
  key <- "Student"
  blank <- tibble::tibble(
    Student         = character(0),
    Last_Attendance = as.Date(integer(0)),
    returnDate      = as.Date(integer(0))
  )

  data <- getCenterData()

  if (action == "upsert") {
    joinedData <- data %>% filterJoin(input, key)

    transformedData <- joinedData %>%
      # RENAME THIS IN TIDY
      mutate(
        Last_Attendance = .data$Last_Attendance_Date
      ) %>%
      select(all_of(names(blank)))

    modifiedCache <-
      dplyr::rows_upsert(cache, transformedData, by = key)
  } else if (action == "delete") {
    modifiedCache <-
      dplyr::rows_delete(cache, input, by = key, unmatched = "ignore")
  }

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
