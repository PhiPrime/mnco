# these functions are intended ONLY for convenience in interactive testing with
#   load_all() (or ctrl+shift+L)
# do NOT use these in a package function. use otherwise at risk of them being
#   removed at any time
gcd  <- function(...) getCenterData(...)
gsr  <- function(...) getStudentRanking(...)
gph  <- function(...) getProgressHistory(...)
pp   <- function(...) plotProgress(...)
cstu <- function() getCenterData("student")
cacc <- function() getCenterData("account")
cenr <- function() getCenterData("enrollment")
cpro <- function() getCenterData("progress")
cass <- function() getCenterData("assessment")
cpay <- function() getCenterData("payment")
ccur <- function() getCenterData("curriculum")
catt <- function() getCenterData("attendance")
ctem <- function() getCenterData("template")
cstu2 <- function() getCenterData("student2")

cardRaffle <- function(width, seed, num = 3) {
  cardWidth <- .1
  numCards <- round(width / cardWidth)

  set.seed(seed)
  winners <- ceiling(runif(num, min = 0, max = numCards))

  return(winners)
}
