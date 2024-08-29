# Create binding for mocking in tests
Sys.Date <- NULL

# For tidy evaluation in a package
utils::globalVariables(".data")

# Dynamic package variables (resets per session!)
the <- new.env(parent = emptyenv())

the$RAW_DATA_DIR <- ifelse(
  all(file.exists("../mcp-data/.git", "../mcp-data/mnco-raw-data")),
  file.path("..", "mcp-data", "mnco-raw-data"),
  file.path(".", "mnco-raw-data")
)
the$CACHE_DIR <- ifelse(
    all(file.exists("../mcp-data/.git", "../mcp-data/mnco-cache")),
    file.path("..", "mcp-data", "mnco-cache"),
    file.path(".", "mnco-cache")
)

the$DOWNLOADS_DIR <- ifelse(
  Sys.info()[["sysname"]] == "Windows" &&
    file.exists(file.path(
      stringr::str_extract(getwd(), "^.*?/.*?/.*?(?=/)"),
      "Downloads"
    )),
  file.path(stringr::str_extract(getwd(), "^.*?/.*?/.*?(?=/)"), "Downloads"),
  NA
)

the$RADIUS_FILE_ROOTS <- c(
  student    = "Students Export",
  account    = "Account Export",
  progress   = "Current Batch Detail Export",
  enrollment = "Enrolled Report",
  assessment =
    "Assessment Report from [0-9]+_[0-9]+_[0-9]+ to [0-9]+_[0-9]+_[0-9]+",
  payment = "Payments.xlsx",
  curriculum = "Curriculum Library Export",
  attendance = "Student Attendance Report Export",
  template   = "Template Export"
)
