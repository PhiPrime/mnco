# these functions are intended ONLY for convenience in interactive testing with
#   load_all() (or ctrl+shift+L)
# do NOT use these in a package function. use otherwise at risk of them being
#   removed at any time
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
#' Recommended workflow:
#'   install()/ctrl+shift+B  (if new code changes are needed)
#'   load_all()/ctrl+shift+L (to use dailyReport() in console)
#'   dailyReport()           (can choose whether to knit/open)
#'
#' New data and updated reports still have to be committed and pushed using
#'   terminal or rstudio gui in mcp-data
#'
#' @param knit Whether to knit
#' @param open Whether to open
#' @noRd
dailyReport <- function(knit = TRUE, open = TRUE) {
  if (knit) rmarkdown::render("../mcp-data/daily-report.Rmd")
  if (open) system2("open", "../mcp-data/daily-report.pdf")
}
