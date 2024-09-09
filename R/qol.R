# these functions are intended ONLY for convenience in interactive testing with
#   load_all() (or ctrl+shift+L)
# do NOT use these in a package function. use otherwise at risk of them being
#   removed at any time
gcd  <- function(...) getCenterData(...)
cstu <- function() getCenterData("student")
cacc <- function() getCenterData("account")
cenr <- function() getCenterData("enrollment")
cpro <- function() getCenterData("progress")
cass <- function() getCenterData("assessment")
cpay <- function() getCenterData("payment")
ccur <- function() getCenterData("curriculum")
catt <- function() getCenterData("attendance")
ctem <- function() getCenterData("template")


#' Knits daily-report.Rmd and opens it
#'
#' @description
#' Recommended workflow in mnco:
#'   edit code
#'   load_all/ctrl+shift+L  (check output interactively, can be skipped)
#'   [repeat above]
#'   install()/ctrl+shift+B
#'   dailyReport()          (can choose whether to knit and/or open)
#'   [repeat from edit]
#'
#' Recommend workflow in mcp-data:
#'   install()/ctrl+shift+B from mnco.Rproj (if changes are needed)
#'   library(mnco)
#'   [once above]
#'   edit .Rmd
#'   dailyReport()
#'   [repeat from edit]
#'
#' New data and updated reports still have to be committed and pushed using
#'   terminal or rstudio gui in mcp-data
#'
#' @param knit Whether to knit
#' @param open Whether to open
#'
#' @export
#' @noRd
dailyReport <- function(knit = TRUE, open = TRUE) {
  if (knit) rmarkdown::render("../mcp-data/daily-report.Rmd")
  if (open) system2("open", "../mcp-data/daily-report.pdf")
}

cardRaffle <- function(width, seed, num = 3) {
  cardWidth <- .1
  numCards <- round(width / cardWidth)

  set.seed(seed)
  winners <- ceiling(runif(num, min = 0, max = numCards))

  return(winners)
}

#' @export
getDataSourceSites <- function(){
  ret <- c("https://radius.mathnasium.com/Student",
           "https://radius.mathnasium.com/CustomerAccount",
           "https://radius.mathnasium.com/ProgressReportManager/CurrentBatchDetail",
           "https://radius.mathnasium.com/AssessmentReport",
           "https://radius.mathnasium.com/Enrollment/EnrollmentReport")
  return(ret)
}


openDataSources <- function(){
  system2("open", getDataSourceSites())
}

#' @export
initialWorkflow <- function(){

  system2("open", "https://radius.mathnasium.com/Account/Login?ReturnUrl=%2F")
  ans <- readline("Did you log in? (Y/N): ")
  if(!stringr::str_detect(toupper(ans), "Y")){
    stop("Next time do the things and say \"Y\".")}

  openDataSources()
  readline("Did you download the files from the opened links? (Y/N): ")
  if(!stringr::str_detect(toupper(ans), "Y")){
    stop("Next time do the things and say \"Y\".")}

  mnco::dailyReport()
}
