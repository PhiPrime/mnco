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
cstu2 <- function() getCenterData("student2")

cardRaffle <- function(width, seed, num = 3) {
  cardWidth <- .1
  numCards <- round(width / cardWidth)

  set.seed(seed)
  winners <- ceiling(runif(num, min = 0, max = numCards))

  return(winners)
}

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
dailyReport <- function(knit = TRUE, open = TRUE, push = FALSE) {
  if (knit) {
    knitTry <- try(rmarkdown::render("../mcp-data/daily-report.Rmd"))

    knitError <- inherits(knitTry, "try-error")
    if (knitError) {
      stop(
        "There was a problem knitting daily-report.Rmd. ",
        "Please debug before knitting again using dailyReport().",
        if (push) "\ndaily-report.pdf will not be committed and pushed automatically."
      )
    }

    if (push) {
      # Commit and push report
      date <- format(Sys.Date(), format = "%m-%d-%Y")

      git_cd("git add .")
      git_cd("git commit -m \"(Auto) ", date, " report\"")
      git_cd("git push")
    }
  }

  if (open) system2("open", "../mcp-data/daily-report.pdf")
}

#' @export
dailyWorkflow <- function(report = TRUE) {
  # Make sure git config user.name and user.email are set in repo for rebase
  #   and push to work
  git_config()

  # Ensure working tree is clean (no uncommitted changes)
  status <- git_cd("git status", intern = T)
  if (!("nothing to commit, working tree clean" %in% status)) {
    stop(
      "You have uncommitted changes. ",
      "Please commit or stash them before trying again:\n\t",
      paste0(status, collapse = "\n\t")
    )
  }

  # Fetch and pull with rebase
  git_cd("git fetch")
  rebase <- git_cd("git rebase origin/main main")

  if (rebase != 0) {
    git_cd("git rebase --abort")
    stop(
      "Conflicts arose when attemping to pull.\n",
      "Please manually pull (with rebase) and resolve conflicts before trying again."
    )
  }

  # Check if daily data is already downloaded
  moveDataDownloads() %>% suppressMessages()
  missing = inherits(try(getCenterData(), silent = T), "try-error")

  if (!missing) {
    message("The daily data is already downloaded. Justin will handle this case later. Please manually commit and push if needed.")
    return(invisible(NULL))
  }

  # Open Radius for user to login
  login = FALSE
  loginURL <- "https://radius.mathnasium.com/Account/UserProfile"
  #"https://radius.mathnasium.com/Account/Login?ReturnUrl=%2F"

  shell.exec(loginURL)
  while (!login) {
    ans <- readline("Did you log in? (Y/N): ")
    if(tolower(ans) != "y") {
      message("Next time do the things and say \"Y\".")
    } else {
      login = TRUE
    }
  }

  # Open Radius for user to download data
  downloaded = FALSE

  system2("open", getDataSources())
  while (missing) {
    ans <- readline("Did you download the files from the opened links? (Y/N): ")

    moveDataDownloads()
    missing = inherits(try(getCenterData(), silent = T), "try-error")

    if(tolower(ans) != "y") {
      message("Next time do the things and say \"Y\".")
    } else if (missing) {
      message("There are still missing files. Make sure they are downloaded. (Justin will figure out how to print which are missing)")
    } else {
      downloaded = TRUE
    }
  }
  if (!downloaded) return(FALSE)

  # Commit and push data
  date <- format(Sys.Date(), format = "%m-%d-%Y")

  git_cd("git add .")
  git_cd("git commit -m \"(Auto) ", date, " data\"")
  git_cd("git push")

  # Knit and commit report
  if (report) dailyReport(push = TRUE)

  return(TRUE)
}

getDataSources <- function() {
  ret <- c(
    "https://radius.mathnasium.com/Student",
    "https://radius.mathnasium.com/CustomerAccount",
    "https://radius.mathnasium.com/ProgressReportManager/CurrentBatchDetail",
    "https://radius.mathnasium.com/Enrollment/EnrollmentReport",
    "https://radius.mathnasium.com/AssessmentReport",
    "https://radius.mathnasium.com/StudentReport"
  )
  return(ret)
}

git_cd <- function(..., path = "../mcp-data", intern = F) {
  command <- paste(..., sep = "")

  stringr::str_replace(command, "git ", paste0("git -C ", path, " ")) %>%
    shell(intern = intern) %>%
    return()
}

git_config <- function() {
  config <- git_cd("git config --list", intern = T)
  missing_name <- !any(stringr::str_detect(config, "^user\\.name"))
  missing_email <- !any(stringr::str_detect(config, "^user\\.email"))

  if (missing_name) {
    name <- readline("git config user.name needs to be set locally for this repository. Enter your GitHub username: ")
    git_cd("git config user.name ", name)
  }
  if (missing_email) {
    email <- readline("git config user.email needs to be set locally for this repository. Enter your GitHub email: ")
    git_cd("git config user.email ", email)
  }
  invisible()
}
