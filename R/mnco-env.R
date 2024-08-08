the <- new.env(parent = emptyenv())
the$RAW_DATA_DIR <- file.path(".", "mnco-raw-data")
the$CACHE_DIR <- file.path(".", "mnco-cache")

getRawDataDir <- function() {
  the$RAW_DATA_DIR
}

getCacheDir <- function() {
  the$CACHE_DIR
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
