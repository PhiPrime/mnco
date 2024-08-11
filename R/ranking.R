#' Title
#'
#' @param date
#'
#' @return
#' @export
#'
#' @examples
getStudentRanking <- function(date = Sys.Date()) {
  # Get the relevant data
  progress <- getCenterData("progress", date) %>%
    dplyr::select("Student", "Skills_Mastered", "Attendances")

  deliveryKey <- getCenterData("enrollment", date) %>%
    dplyr::select("Student", "Monthly_Sessions", "Delivery")

  differentDurationStudents <-
    utils::read.csv(file.path(cacheDir(), "differentDurationStudents.csv"))

  # Merge and filter the data
  dat <- progress %>%
    merge(differentDurationStudents, all.x = T) %>%
    merge(deliveryKey, all.x = T) %>%

    # Scale attendances based on session length
    dplyr::mutate(Duration = dplyr::coalesce(.data$Duration, 60),
           Attendances = .data$Attendances * .data$Duration / 60) %>%

    # Subset valid contestants
    dplyr::filter(.data$Attendances >= .data$Monthly_Sessions / 2,
                  .data$Skills_Mastered > 2,
                  .data$Delivery == "In-Center")

  #Create statistics for based on CI
  CI <- 95
  outlierThreshold <- 4
  roundingDig <- 4

  # Calculate ranking
  dat <- dat %>%
    dplyr::mutate(
      Pest = .data$Skills_Mastered / .data$Attendances,

      #Outlier test
      zscore = (mean(.data$Pest) - .data$Pest) / (stats::sd(.data$Pest) / sqrt(.data$Attendances)),
      samdev = stats::sd(.data$Pest[abs(.data$zscore) < outlierThreshold]),

      UB = round(.data$Pest - stats::qnorm((1 - CI / 100) / 2) *
                   .data$samdev / sqrt(.data$Attendances), roundingDig),
      LB = round(.data$Pest + stats::qnorm((1 - CI / 100) / 2) *
                   .data$samdev / sqrt(.data$Attendances), roundingDig),

      Font_Size = round(32 * .data$LB / max(.data$LB), 1),

      Rank = rank(-.data$LB, ties.method = "min"),
      Rank_Display = paste0(
        .data$Rank,
        dplyr::case_when(
          .data$Rank %% 100 %in% 11:13 ~ "th",
          .data$Rank %% 10 == 1 ~ "st",
          .data$Rank %% 10 == 2 ~ "nd",
          .data$Rank %% 10 == 3 ~ "rd",
          TRUE ~ "th"
        )
      )
    ) %>%
    dplyr::select(-"samdev")

  # Reorder columns and sort by rank
  col_order <- union(
    c("Rank", "Student", "LB", "Pest", "UB", "zscore", "Font_Size", "Rank_Display"),
    names(dat)
  )

  dat <- dat %>%
    dplyr::select(tidyselect::all_of(col_order)) %>%
    dplyr::arrange(.data$Rank)

  return(dat)
}

#' Title
#'
#' @param student
#' @param duration
#'
#' @return
#' @export
#'
#' @examples
addDifferentDurationStudent <- function(student, duration) {
  # need to add file check
  filePath <- file.path(cacheDir(), "differentDurationStudents.csv")
  dat <- utils::read.csv(filePath)

  dat <- rbind(dat[dat$Student != student,], data.frame(Student = student, Duration = duration))
  utils::write.csv(dat, filePath, row.names = F)
}

#' Title
#'
#' @param student
#'
#' @return
#' @export
#'
#' @examples
removeDifferentDurationStudent <- function(student) {
  # need to add file check
  filePath <- file.path(cacheDir(), "differentDurationStudents.csv")
  dat <- utils::read.csv(filePath)

  dat <- dat[dat$Student != student,]
  utils::write.csv(dat, filePath, row.names = F)
}
