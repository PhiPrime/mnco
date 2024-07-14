#### Utility functions:

### radiusDate
as.radiusDate <- function(date) {
  return(paste(month(date), day(date), year(date), sep = "_"))
}#eof
