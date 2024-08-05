as.rawFilePath <- function(dir, root, date) {
  file.path(dir, as.rawFileName(root, date))
}

as.rawFileName <- function(root, date) {
  paste0(root, "  ", as.radiusDate(date), ".xlsx")
}

as.radiusDate <- function(date) {
  paste(lubridate::month(date),
        lubridate::day(date),
        lubridate::year(date), sep = "_")
}
