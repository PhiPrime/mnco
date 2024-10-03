#' @export
dailyWorkflow <- function(report = TRUE) {
  # Make sure git config user.name and user.email are set in repo for rebase
  #   and push to work
  git_config()

  dailyData()

  # Knit and commit report
  if (report) dailyReport(push = TRUE)
}

dailyData <- function() {
  # Ensure working tree is clean (no uncommitted changes)
  status <- git_cd("git status", intern = T)
  if (!("nothing to commit, working tree clean" %in% status)) {
    stop(
      "You have uncommitted changes in mcp-data. ",
      "Please commit or stash them before trying again:\n    ",
      paste0(status, collapse = "\n    ")
    )
  }

  # Fetch and pull with rebase
  git_cd("git fetch")
  rebase <- git_cd("git rebase origin/main main", intern = T)
  rebase_success <- rebase[1] %>%
    stringr::str_detect("(up to date|Successfully rebased)")

  # Handle rebase failure
  if (!rebase_success) {
    git_cd("git rebase --abort", path = "../mnco")
    stop(
      "Conflicts arose when attemping to pull with rebase for mcp-data.\n",
      "The rebase has been aborted:\n    ",
      paste0(rebase, collapse = "\n    "), "\n",
      "Please manually pull (with rebase) and resolve conflicts before trying again."
    )
  }

  # Check if daily data is already downloaded
  moveDataDownloads() %>% suppressMessages()
  missing = try(
    {
      getCenterData()
      getCenterData("assessment")
    },
    silent = T
  ) %>% inherits("try-error")

  if (!missing) {
    message("The daily data is already downloaded. Justin will handle this case later. Please manually commit and push if needed.")
    return(invisible(NULL))
  }

  # Open Radius for user to login
  loginURL <- "https://radius.mathnasium.com/Account/UserProfile"
  shell.exec(loginURL)

  login = FALSE
  while (!login) {
    ans <- readline("Did you log in? (y/n): ")

    while (!(ans %in% c("y", "n"))) {
      ans <- readline("Please enter 'y' or 'n': ")
    }

    if (ans != "y") {
      message("Please log in to Radius.")
    } else {
      login = TRUE
    }
  }

  # Open Radius for user to download data
  downloaded = FALSE

  system2("open", getDataSources())
  while (missing) {
    ans <- readline("Did you download the files from the opened links? (y/n): ")

    while (!(ans %in% c("y", "n"))) {
      ans <- readline("Please enter 'y' or 'n': ")
    }

    moveDataDownloads()
    missing = try(
      {
        getCenterData()
        getCenterData("assessment")
      },
      silent = T
    ) %>% inherits("try-error")

    if(ans != "y") {
      message("Please download the files.")
      missing <- TRUE
    } else if (missing) {
      message(
        "There are still missing files. Make sure they are downloaded. ",
        "(Justin will figure out how to print which are missing)"
      )
    } else {
      downloaded = TRUE
    }
  }

  # Commit and push data
  date <- format(Sys.Date(), format = "%m-%d-%Y")

  git_cd("git add .")
  git_cd("git commit -m \"(Auto) ", date, " data\"")
  git_cd("git push")
}

#' @export
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

getDataSources <- function() {
  c(
    "https://radius.mathnasium.com/Student",
    "https://radius.mathnasium.com/CustomerAccount",
    "https://radius.mathnasium.com/ProgressReportManager/CurrentBatchDetail",
    "https://radius.mathnasium.com/Enrollment/EnrollmentReport",
    "https://radius.mathnasium.com/AssessmentReport",
    "https://radius.mathnasium.com/StudentReport"
  )
}

update_mnco <- function() {
  # Ensure on branch main
  branch <- git_cd("git status", path = "../mnco", intern = T) %>%
    head(1) %>%
    stringr::str_extract("(?<=On branch ).*")

  orig_branch <- NULL
  if (is.na(branch)) {
    stop("Could not find current mnco branch using git status. Please debug.")
  } else if (branch != "main") {
    message("Currently on branch '", branch, "'. Switching to branch 'main'...")
    orig_branch <- branch
    switch <- git_cd("git switch main", path = "../mnco", intern = T)

    if (!("Switched to branch 'main'" %in% switch)) {
      stop(
        "Conflicts arose when attemping to switch to branch 'main' in mnco. ",
        "Please resolve before trying again:\n    ",
        paste0(switch, collapse = "\n    ")
      )
    }
  }

  # Check for updates to mnco
  git_cd("git fetch", path = "../mnco")
  status <- git_cd("git status", path = "../mnco", intern = T)[c(2, 3)]
  change <- status[1] %>% stringr::str_extract("(behind|diverged)")

  if (!is.na(change)) {
    # Get git message for behind or diverged
    if (change == "behind") status <- status[1]

    # Prompt user on how to proceed
    choices <- c(
      "1. Update mnco and continue",
      "2. Don't update mnco and continue",
      "3. Stop the program"
    )

    message(
      "There are updates to mnco:\n    ",
      paste0(status, collapse = "\n    ")
    )

    confirm = "n"
    while (confirm != "y") {
      message(
        "How would you like to proceed?\n    ",
        paste0(choices, collapse = "\n    ")
      )
      update <- readline("Your choice: ")

      while (!(update %in% c(1, 2, 3))) {
        update <- readline("Please select a valid choice: ")
      }

      message(
        "Please confirm your choice:\n    ",
        choices[as.integer(update)]
      )
      confirm <- readline("Confirm (y/n): ")

      while (!(confirm %in% c("y", "n"))) {
        confirm <- readline("Please enter 'y' or 'n': ")
      }
    }

    # Proceed with user choice
    if (update == 1) {
      # Pull with rebase
      rebase <- git_cd("git rebase origin/main main", path = "../mnco", intern = T)
      rebase_success <- rebase[1] %>%
        stringr::str_detect("Successfully rebased")

      # Handle rebase failure
      if (!rebase_success) {
        git_cd("git rebase --abort", path = "../mnco")
        message(
          "Conflicts arose when attemping to pull with rebase for mnco.\n",
          "The rebase has been aborted:\n    ",
          paste0(rebase, collapse = "\n    "), "\n",
          "Consider manually pulling (with rebase) to resolve conflicts."
        )

        # Prompt user on how to proceed
        while (!(update %in% c(2, 3))) {
          message(
            "How would you like to proceed?\n    ",
            paste0(choices[c(2, 3)], collapse = "\n    ")
          )
          update <- readline("Your choice: ")

          while (!(update %in% c(2, 3))) {
            update <- readline("Please select a valid choice: ")
          }
        }
      }
    }
  }

  # Switch back to original branch
  if (!is.null(orig_branch)) {
    message("Switching to back to branch '", orig_branch, "'...")
    switch <- git_cd("git switch ", orig_branch, path = "../mnco")
    if (switch != 0) {
      stop(
        "Could not switch back to branch '", orig_branch, "' in mnco. ",
        "This is unexpected. Please investigate."
      )
    }
  }

  # Stop program on user choice
  if (update == 3) {
    stop("The program has been stopped.")
  }
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
