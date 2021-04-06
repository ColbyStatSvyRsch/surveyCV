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
  cv.svy(NSFG_data, "income~YrEdu", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = FALSE)
  cv.svy(NSFG_data, "income~YrEdu", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = TRUE)
  
  cv.svy(NSFG_data, "income~race", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = FALSE)
  cv.svy(NSFG_data, "income~race", nfolds = 4, strataID = "strata",
         clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = TRUE)
}