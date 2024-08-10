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

radiusFileRoots <- function(
    type = c("all", "types", names(the$RADIUS_FILE_ROOTS))) {
  type <- match.arg(type)

  switch(type,
         "all" = return(the$RADIUS_FILE_ROOTS),
         "types" = return(names(the$RADIUS_FILE_ROOTS)),
         return(the$RADIUS_FILE_ROOTS[[type]])

  )
}
