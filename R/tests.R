if(FALSE) {
  NSFG_data <- read.csv("data-raw/NSFG_data.csv")
  NSFG_data$strata <- as.factor(NSFG_data$strata)
  NSFG_data$SECU <- as.factor(NSFG_data$SECU)
  library(ggplot2)
  ggplot(NSFG_data, aes(x = YrEdu, y = income)) +
    geom_jitter() +
    geom_smooth(method = "loess", se = TRUE) +
    facet_wrap(SECU~., ncol = 2)
  ggplot(NSFG_data, aes(x = YrEdu, y = income)) +
    geom_jitter() +
    geom_smooth(method = "loess", se = TRUE) +
    facet_wrap(strata~., ncol = 2)
  library(splines)
  cv.svy(NSFG_data, "income~ns(YrEdu, df = 5)", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = FALSE)
  cv.svy(NSFG_data, "income~ns(YrEdu, df = 5)", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = TRUE)

  cv.svy(NSFG_data, "income~race", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = FALSE)
  cv.svy(NSFG_data, "income~race", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = TRUE)

  library(dplyr)
  NSFG_hispanic <- NSFG_data %>%
    filter(race == "Hispanic")

  cv.svy(NSFG_hispanic, "income~ns(YrEdu, df = 5)", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 1250, afolds = FALSE)
  cv.svy(NSFG_hispanic, "income~ns(YrEdu, df = 5)", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 1250, afolds = TRUE)

  cv.svy(NSFG_hispanic, "income~race", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 1250, afolds = FALSE)
  cv.svy(NSFG_hispanic, "income~race", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 1250, afolds = TRUE)

  NSFG_black <- NSFG_data %>%
    filter(race == "Black")

  cv.svy(NSFG_black, "income~ns(YrEdu, df = 5)", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 1250, afolds = FALSE)
  cv.svy(NSFG_black, "income~ns(YrEdu, df = 5)", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 1250, afolds = TRUE)

  cv.svy(NSFG_black, "income~race", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 1250, afolds = FALSE)
  cv.svy(NSFG_black, "income~race", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 1250, afolds = TRUE)

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

  NSFG_PreMe <- NSFG_data %>%
    filter(PreMe == "Premature")

  cv.svy(NSFG_PreMe, "income~ns(YrEdu, df = 5)", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 700, afolds = FALSE)

  cv.svy(NSFG_PreMe, "income~ns(YrEdu, df = 5)", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 700, afolds = TRUE)

  ggplot(NSFG_PreMe, aes(x = YrEdu, y = income)) +
    geom_jitter() +
    geom_smooth(method = "loess", se = TRUE) +
    facet_wrap(SECU~., ncol = 2)

  ggplot(NSFG_PreMe, aes(x = YrEdu, y = income)) +
    geom_jitter() +
    geom_smooth(method = "loess", se = TRUE) +
    facet_wrap(strata~., ncol = 2)

}
