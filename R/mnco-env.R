# Create binding for mocking in tests
Sys.Date <- NULL

# For tidy evaluation in a package
utils::globalVariables(".data")

the <- new.env(parent = emptyenv())

the$RAW_DATA_DIR <- file.path(".", "mnco-raw-data")
the$CACHE_DIR <- file.path(".", "mnco-cache")
the$DOWNLOADS_DIR <- ifelse(
  Sys.info()[["sysname"]] == "Windows",
  file.path(stringr::str_extract(getwd(), "^.*?/.*?/.*?(?=/)"), "Downloads"),
  NA
)

the$RADIUS_FILE_ROOTS <- c(
  student = "Students Export",
  account = "Account Export",
  progress = "Current Batch Detail Export",
  enrollment = "Enrolled Report"
)
