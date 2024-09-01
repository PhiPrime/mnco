the_cache <- new.env(parent = emptyenv())

the_cache$FILE_NAMES <- c(
  `center-history` = "center-history.rds",
  `session-length` = "session-length.rds",
  suppression      = "suppressions.rds",
  template         = "templates.rds",
  vacation         = "vacations.rds",
  test             = "test.rds"
)

createCache <- function(type) {
  type <- match.cacheType(type)

  message("createCache(): processCache(empty) called for ", type)
  #processCache(empty, type) %>% saveRDS(cachePath(type))
  invisible(NULL)
}

readCache <- function(type) {
  type <- match.cacheType(type)
  path <- cachePath(type)

  if (!file.exists(path)) {
    message("readCache(): createCache() called for ", type)
    createCache(type)
    return("cache to be created")
  }
  message("readCache(): cleanCache() called for ", type)

  readRDS(path) %>%
  #cleanCache(type) %>%
  return()
}

saveCache <- function(data, type) {
  type <- match.cacheType(type)

  cache <- readCache(type)
  if (!identical(data, cache)) {
    saveRDS(data, cachePath(type))
    return(TRUE)
  }
  return(FALSE)
}

cachePath <- function(type) {
  type <- match.cacheType(type)
  return(file.path(cacheDir(), the_cache$FILE_NAMES[[type]]))
}

match.cacheType <- function(type) {
  return(match.arg(type, names(the_cache$FILE_NAMES)))
}
