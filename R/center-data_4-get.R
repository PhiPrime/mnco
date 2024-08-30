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
getCenterData <- function(type = c("all", radiusFileRoots("types")),
                          date = Sys.Date(), ignoreMissing = F) {
  # Ensure valid type of Radius data file
  type <- match.arg(type)

  # USE match.arg, stopifnot
  if (type == "all") {
    # Get all data and merge
    stu <- getCenterData("student", date)
    acc <- getCenterData("account", date)
    pro <- getCenterData("progress", date)
    enr <- getCenterData("enrollment", date)

    tdat <- stu %>%
      mergeWithFill(acc, .by = "Account_Id") %>%
      merge(pro, all.x = T) %>%
      merge(enr, all.x = T)

  } else {
    # Get and tidy data
    tdat <- readRawData(type, date) %>% tidyRawData(type)
  }

  invisible(tdat)
}

#' Combine data frames and coalesce shared columns
#'
#' @param df1,df2 Data frames to merge. Elements in `df1` have priority for
#'  coalescing
#' @param .by Column name to use for merging through [merge()]
#'
#' @return A data frame
#' @export
#'
#' @examples
#' stu <- getCenterData("student")
#' acc <- getCenterData("account")
#' mergeWithFill(stu, acc, .by = "Account_Id")
mergeWithFill <- function(df1, df2, .by) {
  # Merge df1 and df2. Common columns are suffixed with .x and .y
  df <- merge(df1, df2, all.x = T, by = .by)

  # Iterate through common columns
  for (colName in intersect(names(df1), names(df2))) {
    # Skip iteration if column was used to match
    if (colName %in% .by) next

    # Suffixed column strings
    colName.x <- paste0(colName, ".x")
    colName.y <- paste0(colName, ".y")

    # Fill value for common column to col.x and rename to col
    df[[colName.x]] <- dplyr::coalesce(df[[colName.x]], df[[colName.y]])
    names(df)[names(df) == colName.x] <- colName
    # CHANGE TO THIS? MAYBE DOESN'T WORK
    #df <- rename(df, col = col.x)

    # Delete col.y
    df[[colName.y]] <- NULL
  }

  return(df)
}
