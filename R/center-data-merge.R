#' Title
#'
#' @param df1
#' @param df2
#' @param .by
#'
#' @return
#' @export
#'
#' @examples
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
