#' @export
dailyWorkflow <- function(update = TRUE, report = TRUE) {
  # Make sure git config user.name and user.email are set in repo for rebase
  #   and push to work
  git_config(path = "../mnco")
  git_config(path = "../mcp-data")

  # Update mnco
  if (update) {
    mnco_updated <- update_mnco()

    if (mnco_updated) {
      restart <- c(
        "install.packages(\"../mnco\", repos = NULL, type = \"source\")",
        "writeLines(\"mnco::dailyWorkflow(update = F)\", con = \"clipboard\", sep = \"\")",
        "message(\"Hit Ctrl+V and Enter to continue.\")"
      )
      rstudioapi::restartSession(restart)
    }
  }

  # Download and commit data
  data_pushed <- dailyData()

  # Knit and commit report
  push_report <- TRUE
  if (!data_pushed && report) {
    push_report <- prompt_user(
      msg = c(
        "Does the daily report still need to be knitted, committed, and pushed?\n",
        tab_message(c(
          "1. Push (check git log first!)",
          "2. Don't push"
        ))
      ),
      choices = c(1, 2),
      prompt1 = "Your choice: ",
      prompt2 = "Please select a valid choice: "
    )

    if (push_report == 1) {
      push_report <- TRUE
    } else {
      push_report <- FALSE
    }
  }

  if (report) dailyReport(push = push_report)
}

dailyData <- function() {
  # Ensure working tree is clean (no uncommitted changes)
  status <- git_cd("git status", path = "../mcp-data", intern = T)
  if (!("nothing to commit, working tree clean" %in% status)) {
    stop(
      "You have uncommitted changes in mcp-data. ",
      "Please commit or stash them before trying again:\n",
      tab_message(status), "\n"
    )
  }

  # Fetch and pull with rebase
  git_cd("git fetch", path = "../mcp-data")
  rebase <- git_cd("git rebase origin/main main", path = "../mcp-data", intern = T)
  rebase_success <- rebase %>%
    stringr::str_detect("(up to date|Successfully rebased)") %>%
    any()

  # Handle rebase failure
  if (!rebase_success) {
    git_cd("git rebase --abort", path = "../mnco")
    stop(
      "Conflicts arose when attemping to pull with rebase for mcp-data.\n",
      "The rebase has been aborted:\n",
      tab_message(rebase), "\n\n",
      "Please manually pull (with rebase) and resolve conflicts before trying again."
    )
  }

  # Check if daily data is already downloaded
  moveDataDownloads() %>% suppressMessages()
  missing = try(
    {
      getCenterData()
      getCenterData("assessment")
      getCenterData("attendance")
    },
    silent = T
  ) %>% inherits("try-error")

  push = TRUE
  if (!missing) {
    push <- prompt_user(
      msg = c(
        "The daily data is already downloaded. Does it still need to be committed and pushed?\n",
        tab_message(c(
          "1. Push (check git log first!)",
          "2. Don't push"
        ))
      ),
      choices = c(1, 2),
      prompt1 = "Your choice: ",
      prompt2 = "Please select a valid choice: "
    )

    if (push == 1) {
      push = TRUE
    } else {
      push = FALSE
    }
  }

  # Download daily data from Radius
  if (missing) {
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
    system2("open", getDataSources())
    while (missing) {
      ans <- readline("Did you download the files from the opened links? (y/n): ")

      while (!(ans %in% c("y", "n"))) {
        ans <- readline("Please enter 'y' or 'n': ")
      }

      if (ans == "y") {
        moveDataDownloads()
        missing = try(
          {
            getCenterData()
            getCenterData("assessment")
            getCenterData("attendance")
          },
          silent = T
        ) %>% inherits("try-error")

        if (missing) {
          message(
            "There are still missing files. Make sure they are downloaded. ",
            "(Justin will figure out how to print which are missing)"
          )
        }
      } else if (ans == "n") {
        message("Please download the files.")
        missing <- TRUE
      }
    }
  }

  # Commit and push data
  if (push) {
    date <- format(Sys.Date(), format = "%m-%d-%Y")

    git_cd("git add .", path = "../mcp-data")
    commit = git_cd("git commit -m \"(Auto) ", date, " data\"", path = "../mcp-data", intern = T)

    if ("nothing to commit, working tree clean" %in% commit) {
      message("There is no new data to commit and push to mcp-data.")
      push <- FALSE
    }
  }
  if (push) git_cd("git push", path = "../mcp-data")

  return(push)
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

      git_cd("git add .", path = "../mcp-data")
      git_cd("git commit -m \"(Auto) ", date, " report\"", path = "../mcp-data")
      git_cd("git push", path = "../mcp-data")
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
    "https://radius.mathnasium.com/StudentReport",
    "https://radius.mathnasium.com/StudentAttendanceMonthlyReport"
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
        "Please resolve before trying again:\n",
        tab_message(switch)
      )
    }
  }

  # Check for updates to mnco
  git_cd("git fetch", path = "../mnco")
  status <- git_cd("git status", path = "../mnco", intern = T)[c(2, 3)]
  change <- status[1] %>% stringr::str_extract("(behind|diverged)")

  # 0 - no updates, 1 - update request, 2 - update denied, 3 - stop program
  update <- 0

  # Prompt user if update available
  if (!is.na(change)) {
    # Get git message for behind or diverged
    if (change == "behind") status <- status[1]

    update_options <- c(
      "1. Update mnco and continue",
      "2. Don't update mnco and continue",
      "3. Stop the program"
    )

    message(
      "There are updates to mnco:\n",
      tab_message(status), "\n"
    )

    update <- prompt_user(
      choices = c(1, 2, 3),
      msg = c(
        "How would you like to proceed?\n",
        tab_message(update_options)
      ),
      prompt1 = "Your choice: ",
      prompt2 = "Please select a valid choice: "
    )
  }

  # Ensure working tree is clean (no uncommitted changes)
  if (update == 1) {
    status <- git_cd("git status", intern = T, path = "../mnco")
    while (!("nothing to commit, working tree clean" %in% status) && update == 1) {
      message(
        "You have uncommitted changes in mnco. ",
        "They must be committed or stashed before continuing:\n",
        tab_message(status), "\n"
      )

      uncommitted_options <- c(
        "1. Open mnco.Rproj to commit/stash then continue (tell Justin if you want it to stash automatically)",
        "2. Don't update mnco and continue",
        "3. Stop the program"
      )

      update <- prompt_user(
        choices = c(1, 2, 3),
        msg = c(
          "How would you like to proceed?\n",
          tab_message(uncommitted_options)
        ),
        prompt1 = "Your choice: ",
        prompt2 = "Please select a valid choice: "
      )

      if (update == 1) {
        message("Return to this window to confirm after committing or stashing your changes.")
        Sys.sleep(1.5)
        system2("open", "../mnco/mnco.Rproj")

        prompt_user(
          choices = "y",
          prompt1 = "Confirm (y): ",
          prompt2 = "Please enter 'y': "
        )

        status <- git_cd("git status", path = "../mnco", intern = T)
      }
    }
  }

  # Attempt to pull with rebase
  if (update == 1) {
    rebase <- git_cd("git rebase origin/main main", path = "../mnco", intern = T)
    rebase_success <- rebase %>%
      stringr::str_detect("Successfully rebased") %>%
      any()

    # Handle rebase failure
    if (!rebase_success) {
      git_cd("git rebase --abort", path = "../mnco")
      message(
        "Conflicts arose when attemping to pull with rebase for mnco.\n",
        "The rebase has been aborted:\n",
        tab_message(rebase), "\n\n",
        "Consider manually pulling (with rebase) to resolve conflicts."
      )

      # Prompt user on how to proceed
      update <- prompt_user(
        choices = c(2, 3),
        msg = c(
          "How would you like to proceed?\n",
          tab_message(update_options[c(2, 3)])
        ),
        prompt1 = "Your choice: ",
        prompt2 = "Please select a valid choice: "
      )
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

  if (update == 0 || update == 2) {
    # Continue without updates
    return(FALSE)
  } else if (update == 1) {
    # Reinstall mnco and continue
    return(TRUE)
  } else if (update == 3) {
    # Stop program on user choice
    stop("The program has been stopped.")
  }
}

git_cd <- function(..., path, intern = F) {
  command <- paste(..., sep = "")

  stringr::str_replace(command, "git ", paste0("git -C ", path, " ")) %>%
    shell(intern = intern) %>%
    return()
}

git_config <- function(path) {
  config <- git_cd("git config --list", path = path, intern = T)
  repo <- path %>% stringr::str_replace("../", "")

  missing_name <- !any(stringr::str_detect(config, "^user\\.name"))
  missing_email <- !any(stringr::str_detect(config, "^user\\.email"))

  if (missing_name) {
    name <- paste0("git config user.name needs to be set locally for ", repo, ". Enter your GitHub username: ") %>%
      readline()
    git_cd("git config user.name ", path = path, name)
  }
  if (missing_email) {
    email <- paste0("git config user.email needs to be set locally for ", repo, ". Enter your GitHub email: ") %>%
      readline()
    git_cd("git config user.email ", path = path, email)
  }
  invisible()
}

tab_message <- function(message) {
  paste0(
    "    ",
    paste0(message, collapse = "\n    ")
  )
}

prompt_user <- function(choices, msg = NULL, prompt1 = NULL, prompt2 = NULL) {
  if (!is.null(msg)) message(msg)

  input <- readline(prompt1)
  while(!(input %in% choices)) {
    validation_prompt = if (!is.null(prompt2)) prompt2 else prompt1
    input <- readline(validation_prompt)
  }

  return(input)
}
