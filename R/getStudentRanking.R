getStudentRanking <- function(dir, date = Sys.Date()) {
  # Get the relevant data
  progress <- getCenterData(dir, "progress", date) %>%
    select(Student, Skills_Mastered, Attendances)

  deliveryKey <- getCenterData(dir, "enrollment", date) %>%
    select(Student, Monthly_Sessions, Delivery)

  differentDurationStudents <-
    read.csv("Cache/differentDurationStudents.csv")

  # Merge and filter the data
  dat <- progress %>%
    merge(differentDurationStudents, all.x = T) %>%
    merge(deliveryKey, all.x = T) %>%

    # Scale attendances based on session length
    mutate(Duration = coalesce(Duration, 60),
           Attendances = Attendances * Duration / 60) %>%

    # Subset valid contestants
    filter(Attendances >= Monthly_Sessions / 2,
           Skills_Mastered > 2,
           Delivery == "In-Center")

  #Create statistics for based on CI
  CI <- 95
  outlierThreshold <- 4
  roundingDig <- 4

  # Calculate ranking
  dat <- dat %>%
    mutate(
      Pest = Skills_Mastered / Attendances,

      #Outlier test
      zscore = (mean(Pest) - Pest) / (sd(Pest) / sqrt(Attendances)),
      samdev = sd(Pest[abs(zscore) < outlierThreshold]),

      UB = round(Pest - qnorm((1 - CI / 100) / 2) *
                   samdev / sqrt(Attendances), roundingDig),
      LB = round(Pest + qnorm((1 - CI / 100) / 2) *
                   samdev / sqrt(Attendances), roundingDig),

      Font_Size = round(32 * LB / max(LB), 1),

      Rank = rank(-LB, ties.method = "min"),
      Rank_Display = paste0(
        Rank,
        case_when(
          Rank %% 100 %in% 11:13 ~ "th",
          Rank %% 10 == 1 ~ "st",
          Rank %% 10 == 2 ~ "nd",
          Rank %% 10 == 3 ~ "rd",
          TRUE ~ "th"
        )
      )
    ) %>%
    select(-samdev)

  # Reorder columns and sort by rank
  col_order <- union(
    c("Rank", "Student", "LB", "Pest", "UB", "zscore", "Font_Size", "Rank_Display"),
    names(dat)
  )

  dat <- dat %>%
    select(all_of(col_order)) %>%
    arrange(Rank)

  return(dat)
}

addDifferentDurationStudent <- function(student, duration) {
  # need to add file check
  filePath <- file.path(getwd(), "Cache/differentDurationStudents.csv")
  dat <- read.csv(filePath)

  dat <- rbind(dat, data.frame(Student = student, Duration = duration))
  write.csv(dat, filePath, row.names = F)
}

removeDifferentDurationStudent <- function(student) {
  # need to add file check
  filePath <- file.path(getwd(), "Cache/differentDurationStudents.csv")
  dat <- read.csv(filePath)

  dat <- dat[dat$Student != student,]
  write.csv(dat, filePath, row.names = F)
}
