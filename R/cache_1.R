the_cache <- new.env(parent = emptyenv())

the_cache$FILE_NAMES <- c(
  `center-history` = "center-history.rds",
  `session-length` = "session-length.rds",
  suppression      = "suppressions.rds",
  template         = "templates.rds",
  vacation         = "vacations.rds",
  test             = "test.rds"
)

cachePath <- function(type) {
  match.arg(type, names(the_cache$FILE_NAMES))
  return(file.path(cacheDir(), the_cache$FILE_NAMES[[type]]))
}
