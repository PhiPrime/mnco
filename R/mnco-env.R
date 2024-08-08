the <- new.env(parent = emptyenv())
the$RAW_DATA_DIR <- file.path(".", "mnco-raw-data")
the$CACHE_DIR <- file.path(".", "mnco-cache")

rawDataDir <- function() {
  dir <- the$RAW_DATA_DIR
  if (!file.exists(dir)) {
    stop("`RAW_DATA_DIR` does not exist: ", dir, ".\n",
         "Call setRawDataDir() to set a valid directory.")
  }
  dir
}

cacheDir <- function() {
  dir <- the$CACHE_DIR
  if (!file.exists(dir)) {
    stop("`CACHE_DIR` does not exist: ", dir, ".\n",
         "Call setCacheDir() to set a valid directory.")
  }
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
