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

the$RADIUS_FILE_ROOTS <- list(
  student = "Students Export",
  account = "Account Export",
  progress = "Current Batch Detail Export",
  enrollment = "Enrolled Report"
)

rawDataDir <- function() {
  path <- the$RAW_DATA_DIR
  if (!file.exists(path)) {
    stop("`RAW_DATA_DIR` does not exist: ", path, ".\n",
         "Call setRawDataDir() to set a valid directory.")
  }
  path
}

setRawDataDir <- function(path) {
  old <- the$RAW_DATA_DIR
  the$RAW_DATA_DIR <- path
  invisible(old)
}

cacheDir <- function() {
  path <- the$CACHE_DIR
  if (!file.exists(path)) {
    stop("`CACHE_DIR` does not exist: ", path, ".\n",
         "Call setCacheDir() to set a valid directory.")
  }
  path
}

setCacheDir <- function(path) {
  old <- the$CACHE_DIR
  the$CACHE_DIR <- path
  invisible(old)
}

downloadsDir <- function() {
  path <- the$DOWNLOADS_DIR
  if (is.na(path)) {
    stop("`DOWNLOADS_DIR` has not been set.\n",
         "Call setDownloadsDir() to set a valid directory.")
  } else if (!file.exists(path)) {
    stop("`DOWNLOADS_DIR` does not exist: ", path, ".\n",
         "Call setDownloadsDir() to set a directory.")
  }
  path
}

setDownloadsDir <- function(path) {
  old <- the$DOWNLOADS_DIR
  the$DOWNLOADS_DIR <- path
  invisible(old)
}

radiusFileRoots <- function(
    type = c("all", "types", names(the$RADIUS_FILE_ROOTS))) {
  type <- match.arg(type)

  switch(type,
    "all" = return(the$RADIUS_FILE_ROOTS),
    "types" = return(names(the$RADIUS_FILE_ROOTS)),
    return(the$RADIUS_FILE_ROOTS[[type]])

  )
}
