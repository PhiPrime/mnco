Sys.Date <- NULL

the <- new.env(parent = emptyenv())
the$RAW_DATA_DIR <- file.path(".", "mnco-raw-data")
the$CACHE_DIR <- file.path(".", "mnco-cache")
the$DOWNLOADS_DIR <- ifelse(
  Sys.info()[["sysname"]] == "Windows",
  file.path(stringr::str_extract(getwd(), "^.*?/.*?/.*?(?=/)"), "Downloads"),
  NA
)

rawDataDir <- function() {
  path <- the$RAW_DATA_DIR
  if (!file.exists(path)) {
    stop("`RAW_DATA_DIR` does not exist: ", path, ".\n",
         "Call setRawDataDir() to set a valid directory.")
  }
  path
}

cacheDir <- function() {
  path <- the$CACHE_DIR
  if (!file.exists(path)) {
    stop("`CACHE_DIR` does not exist: ", path, ".\n",
         "Call setCacheDir() to set a valid directory.")
  }
  path
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

setRawDataDir <- function(path) {
  old <- the$RAW_DATA_DIR
  the$RAW_DATA_DIR <- path
  invisible(old)
}

setCacheDir <- function(path) {
  old <- the$CACHE_DIR
  the$CACHE_DIR <- path
  invisible(old)
}

setDownloadsDir <- function(path) {
  old <- the$DOWNLOADS_DIR
  the$DOWNLOADS_DIR <- path
  invisible(old)
}
