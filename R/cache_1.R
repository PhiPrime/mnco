the_cache <- new.env(parent = emptyenv())

the_cache$FILE_NAMES <- c(
  `center-history` = "center-history.rds",
  `session-length` = "session-length.rds",
  suppression      = "suppressions.rds",
  template         = "templates.rds",
  vacation         = "vacations.rds"
)

