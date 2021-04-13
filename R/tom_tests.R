if(FALSE) {
  NSFG_data <- read.csv("data-raw/NSFG_data.csv")
  NSFG_data$strata <- as.factor(NSFG_data$strata)
  NSFG_data$SECU <- as.factor(NSFG_data$SECU)
  library(ggplot2)
  library(dplyr)
  library(splines)
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
##everything above here is residual tests cole was working on

#cole tried breakdown by race so I'm trying eduCat
NSFG_data %>%
  count(eduCat)

#there were abot 1500 in HS or GED so I chose that one
HSNSFG <- NSFG_data[NSFG_data$eduCat=="HS or GED",]


HSNSFG %>%
  count(SECU)

HSNSFG %>%
  count(strata)

#both the strat and clusters have a decent amount of observations in each group so this could be a good place to dive in deeper

cv.svy(HSNSFG, "income~race+YrEdu", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = FALSE)
cv.svy(HSNSFG, "income~race+YrEdu", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = TRUE)

#this shows some difference but not a whole ton

cv.svy(HSNSFG, "income~race+BMI", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = FALSE)
cv.svy(HSNSFG, "income~race+BMI", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = TRUE)
#again above were still within one SD

HSNSFG$PreMeInd <- 0
HSNSFG$PreMeInd[HSNSFG$PreMe == "Term"] <- 1
cv.svy(HSNSFG, "PreMeInd~race+income+Wanted", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = FALSE, method = "logistic")

cv.svy(HSNSFG, "PreMeInd~race+income+Wanted", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = TRUE, method = "logistic")

#here we are getting kinda close

cv.svy(HSNSFG, "PreMeInd~race+income+Wanted", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 2500, afolds = FALSE, method = "logistic")

cv.svy(HSNSFG, "PreMeInd~race+income+Wanted", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 2500, afolds = TRUE, method = "logistic")

#couldn't really find anything here so i tried re grouping along some college

BSNSFG <-NSFG_data[NSFG_data$eduCat=="Bachelors",]


BSNSFG$PreMeInd <- 0
BSNSFG$PreMeInd[BNSFG$PreMe == "Term"] <- 1
cv.svy(BSNSFG, "income~race+YrEdu", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = FALSE)
cv.svy(BSNSFG, "income~race+YrEdu", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = TRUE)

#this shows some difference but not a whole ton

cv.svy(BSNSFG, "income~race+BMI", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = FALSE)
cv.svy(BSNSFG, "income~race+BMI", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = TRUE)
#maybe a bit more difference

cv.svy(BSNSFG, "PreMeInd~race+income+Wanted", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = FALSE, method = "logistic")

cv.svy(BSNSFG, "PreMeInd~race+income+Wanted", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = TRUE, method = "logistic")

#not super close here


#lets try a different breakdown
SCNSFG <-NSFG_data[NSFG_data$eduCat=="Some college",]

# not enough PSU
