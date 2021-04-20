if(FALSE) {
  NSFG_data <- read.csv("data-raw/NSFG_data.csv")
  NSFG_data$strata <- as.factor(NSFG_data$strata)
  NSFG_data$SECU <- as.factor(NSFG_data$SECU)
  library(ggplot2)
  ggplot(NSFG_data, aes(x = YrEdu, y = income)) +
    geom_jitter() +
    geom_smooth(method = "loess", se = TRUE) +
    facet_wrap(SECU~., ncol = 2) +
    labs(x = "Years Educated", y = "Income (Expressed as % of Poverty Level)",
         title = "Relationship Separated by Cluster")
  ggplot(NSFG_data, aes(x = YrEdu, y = income)) +
    geom_jitter() +
    geom_smooth(method = "loess", se = TRUE) +
    facet_wrap(strata~., ncol = 2) +
    labs(x = "Years Educated", y = "Income (Expressed as % of Poverty Level)",
         title = "Relationship Separated by Stratum")
  library(splines)

  # These two examples are interesting because the MSEs are pretty different, but the SE is very different.
  # They are worth exploring further, but this is not done on any subset of data.

  cv.svy(NSFG_data, "income~ns(YrEdu, df = 5)", nfolds = 4, weights = "wgt", N = 5100)
  cv.svy(NSFG_data, "income~ns(YrEdu, df = 5)", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100)

  cv.svy(NSFG_data, "income~race", nfolds = 4, weights = "wgt", N = 5100)
  cv.svy(NSFG_data, "income~race", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100)

  library(dplyr)
  NSFG_hispanic <- NSFG_data %>%
    filter(race == "Hispanic")

  # These were examples of using possible subsets of data,
  # but each one cut down the amount of observations in at least one stratum or cluster,
  # that the function could not run.

  cv.svy(NSFG_hispanic, "income~ns(YrEdu, df = 5)", nfolds = 4, weights = "wgt", N = 1250)
  cv.svy(NSFG_hispanic, "income~ns(YrEdu, df = 5)", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 1250)

  cv.svy(NSFG_hispanic, "income~race", nfolds = 4, weights = "wgt", N = 1250)
  cv.svy(NSFG_hispanic, "income~race", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 1250)

  NSFG_black <- NSFG_data %>%
    filter(race == "Black")

  cv.svy(NSFG_black, "income~ns(YrEdu, df = 5)", nfolds = 4, weights = "wgt", N = 1250)
  cv.svy(NSFG_black, "income~ns(YrEdu, df = 5)", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 1250)

  cv.svy(NSFG_black, "income~race", nfolds = 4, weights = "wgt", N = 1250)
  cv.svy(NSFG_black, "income~race", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 1250)

  NSFG_data %>%
    count(SECU)

  NSFG_data %>%
    count(strata)

  NSFG_black %>%
    count(SECU)

  NSFG_black %>%
    count(strata)

  NSFG_hispanic %>%
    count(SECU)

  NSFG_hispanic %>%
    count(strata)

  NSFG_data %>%
    count(eduCat)

  NSFG_data %>%
    count(PreMe)

  # This subset of observations allows the functions to run,
  # but there is no discernible difference between when we account for the correct survey design or not.

  NSFG_PreMe <- NSFG_data %>%
    filter(PreMe == "Premature")

  cv.svy(NSFG_PreMe, "income~ns(YrEdu, df = 5)", nfolds = 4, weights = "wgt", N = 700)

  cv.svy(NSFG_PreMe, "income~ns(YrEdu, df = 5)", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 700)

  ggplot(NSFG_PreMe, aes(x = YrEdu, y = income)) +
    geom_jitter() +
    geom_smooth(method = "loess", se = TRUE) +
    facet_wrap(SECU~., ncol = 2)

  ggplot(NSFG_PreMe, aes(x = YrEdu, y = income)) +
    geom_jitter() +
    geom_smooth(method = "loess", se = TRUE) +
    facet_wrap(strata~., ncol = 2)

}
