the_cache <- new.env(parent = emptyenv())

the_cache$FILE_NAMES <- c(
  `center-history` = "center-history.rds",
  `session-length` = "session-length.rds",
  suppression      = "suppressions.rds",
  template         = "templates.rds",
  vacation         = "vacations.rds",
  test             = "test.rds"
)

readCache <- function(type) {
  match.arg(type, names(the_cache$FILE_NAMES))
  path <- cachePath(type)

  if (!file.exists(path)) {
    message("readCache(): createCache() called for ", type)
    #createCache(type)
    return(NULL)
  }
  message("readCache(): cleanCache() called for ", type)

  return(readRDS(path))

}

cachePath <- function(type) {
  match.arg(type, names(the_cache$FILE_NAMES))
  return(file.path(cacheDir(), the_cache$FILE_NAMES[[type]]))
}
