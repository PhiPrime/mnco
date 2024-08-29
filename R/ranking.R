#' Get student ranking data
#'
#' @param date Date to calculate rankings for
#'
#' @return A data frame
#' @export
#'
#' @examples
#' getStudentRanking()
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

#' Assign session length to student
#'
#' This function is used for assigning a session length other than the default
#' 60 minutes to a student. The student and their session length is stored in
#' `differentDurationStudents.rds` in the cache directory specified by
#' [cacheDir()].
#'
#' @param student Name of student to add
#' @param duration Number of minutes
#'
#' @return None (invisible `NULL`)
#' @export
#'
#' @examples
#' addDifferentDurationStudent("John Doe", 90)
addDifferentDurationStudent <- function(student, duration) {
  # need to add file check
  filePath <- file.path(cacheDir(), "differentDurationStudents.csv")
  dat <- utils::read.csv(filePath)

  dat <- rbind(dat[dat$Student != student,], data.frame(Student = student, Duration = duration))
  utils::write.csv(dat, filePath, row.names = F)
}

#' Unassign non-default session length from student
#'
#' See [addDifferentDurationStudent()] for more details.
#'
#' @param student Name of student to remove
#'
#' @return None (invisible `NULL`)
#' @export
#'
#' @examples
#' removeDifferentDurationStudent("John Doe")
removeDifferentDurationStudent <- function(student) {
  # need to add file check
  filePath <- file.path(cacheDir(), "differentDurationStudents.csv")
  dat <- utils::read.csv(filePath)

  dat <- dat[dat$Student != student,]
  utils::write.csv(dat, filePath, row.names = F)
}

#Used with ranking to adjust score based on variableName
#' Title
#'
#' @param dat
#' @param variableName
#' @param centerVal
#'
#' @return
#' @export
#'
#' @examples
regularizeScore <- function(dat, variableName, centerVal){
  #if no Score present in data frame, assume it should be LB
  if(!("Score" %in% names(dat))){
    dat <- dplyr::mutate(dat, Score = .data$LB)
  }

  gdMeans <- t(sapply(unique(dat[[variableName]]), function(x)
    data.frame(variableName=x,
               mean= mean(dat[dat[[variableName]]==x,]$Pest))))
  gdMeans <- data.frame(tmp = unlist(gdMeans[,1]),
                        Mean = unlist(gdMeans[,2]))
  names(gdMeans)[names(gdMeans) == "tmp"] <- variableName

  gdMeans <- gdMeans[order(gdMeans[[variableName]]),]
  gdMeans <- dplyr::mutate(gdMeans,
                           offset = gdMeans[gdMeans[[variableName]]==centerVal,
                           ]$Mean-.data$Mean)
  dat <- merge(dat, gdMeans)
  dat$Score <- with(dat, Score + offset)
  dat <- dplyr::select(dat, -"offset", -"Mean")
  return(dat)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
showcaseRegularizeScore <- function(){
  dat <- merge(getStudentRanking(),
               getMostRecentAssessments())

  #Force -3 to be min difference considered
  dat[which(dat$gradeDif<(-3)),]$gradeDif <- -3

  p1 <- ggplot2::ggplot(dat, ggplot2::aes(x=.data$gradeDif, y = .data$LB)) +
    ggplot2::ylab("Score") +
    ggplot2::geom_point() + ggplot2::geom_smooth() + ggplot2::ggtitle("No Regularization")


  dat <- regularizeScore(dat,"Level", 4)#  "gradeDif", 0)

  p2 <- ggplot2::ggplot(dat, ggplot2::aes(x=.data$gradeDif, y = .data$Score)) +
    ggplot2::geom_point() + ggplot2::geom_smooth() + ggplot2::ggtitle("Regularized on gradeDif")


  dat <- regularizeScore(dat,  "gradeDif", 0)#"Level", 4)

  p3 <- ggplot2::ggplot(dat, ggplot2::aes(x=.data$gradeDif, y = .data$Score)) +
    ggplot2::geom_point() + ggplot2::geom_smooth() +
    ggplot2::ggtitle("Regularized on gradeDif & assessmentLevel")


  gridExtra::grid.arrange(p1,p2,p3,ncol=1)
  #return(dat)
}
