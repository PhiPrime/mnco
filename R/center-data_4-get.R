#' Read and tidy Radius data
#'
#' @param type String of which Radius data to read
#' @param date Date to read data for
#' @param ignoreMissing `logical` indicating to return empty data frame
#'  (instead of signaling an error) if data file not found
#'
#' @return A data frame
#' @export
#'
#' @examples
#' getCenterData()
#' getCenterData("student")
getCenterData <- function(type = "all", date = Sys.Date(), ignoreMissing = F) {
  # Ensure valid type of Radius data file
  typeChoices = radiusFileRoots("types")
  stopifnot(lubridate::is.Date(date))

  if (length(type) == 1) {
    type <- match.arg(type, c("all", typeChoices))
  } else {
    matchLength = length(
      try(match.arg(type, typeChoices, several.ok = T), silent = T)
    )

    if (length(type) != matchLength) {
      stop(
        "If `type` has length > 1, elements should be one of ",
        radiusFileRoots("types") %>%
          paste0("\"", ., "\"", collapse = ", ")
      )
    }
  }

  # Get data
  if (identical(type, "all")) {
    # Get all data and merge
    data <- list(
      getCenterData("student", date),
      getCenterData("account", date),
      getCenterData("progress", date),
      getCenterData("enrollment", date),
      getStudentRanking(date)
    )
  } else if (length(type) > 1) {
    data <- lapply(type, getCenterData, date = date)
  } else {
    # Read and tidy data
    data <- readRawData(type, date) %>% tidyRawData(type)
  }

  # Join and return data frames
  patchJoin(data, .by = c("Student", "Account_Id"), first = T)
}

#' Join data frames and patch NA values
#'
#' @param ... Data frames or list of data frames
#' @param .by Character string or vector of key columns used to join
#' @param first If `TRUE`, only the shared column that appears first in `.by` is
#'   used to join? Matching occurs between each pair of consecutive data frames
#'   in the order they are passed in. If `FALSE`, all key columns in `.by` must
#'   must exist in all data frames.
#'
#' @return A data frame
#' @keywords internal
#'
#' @examples
#' stu <- getCenterData("student")
#' acc <- getCenterData("account")
#' pro <- getCenterData("progress")
#' patchJoin(stu, acc, .by = "Account_Id")
#' patchJoin(list(stu, acc), .by = "Account_Id")
#' patchJoin(stu, acc, pro, .by = c("Student", "Account_Id"), first = T)
#'
patchJoin <- function(..., .by, first = FALSE) {
  # Data frames can be passed in singly or in a list
  dfs <- list(...)
  if (length(dfs) == 1 && isa(dfs[[1]], "list")) dfs <- dfs[[1]]

  # Join data frames one by one
  joined <- NULL

  for (i in seq_along(dfs)) {
    toJoin <- dfs[[i]]

    # Store first data frame
    if (is.null(joined)) {
      joined <- toJoin
      next
    }

    # Ensure key columns from `.by` exist between `joined` and `toJoin`
    sharedCols <- intersect(names(joined), names(toJoin))

    if (first) {
      # Join only by the first shared column in `.by`
      byMatch <-
        match(T, .by %in% sharedCols) %>%
        magrittr::extract(.by, .)

      # Throw error if no single column found in both data frames
      if (is.na(byMatch)) {
        stop(
          "None of the following key columns could be found in `patchJoin()` ",
          "arguments ", (i - 1), " and ", i, ":\n",
          "\t", paste0(.by, collapse = " ")
        )
      }
    } else {
      # Join by all columns in `.by`
      byMatch <- .by

      # Throw error if all columns not found in both data frames
      if (!all(byMatch %in% sharedCols)) {
        stop(
          "All of the following key columns were not found in `patchJoin()` ",
          "arguments ", (i - 1), " and ", i, ":\n",
          "\t", paste0(.by, collapse = " "), "\n",
          "  Did you mean to use `first = TRUE` to join by the first shared ",
          "column in `.by`?"
        )
      }
    }

    # Join data frames and patch NA values
    joined <- joined %>%
      dplyr::left_join(toJoin, by = byMatch, suffix = c("", ".y")) %>%
      select(-dplyr::ends_with(".y")) %>%
      dplyr::rows_patch(toJoin, by = byMatch, unmatched = "ignore")
  }

  invisible(joined)
}
