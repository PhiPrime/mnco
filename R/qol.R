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
