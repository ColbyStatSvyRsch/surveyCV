if(FALSE) {
  NSFG_data <- read.csv("data-raw/NSFG_data.csv")
  NSFG_data$strata <- as.factor(NSFG_data$strata)
  NSFG_data$SECU <- as.factor(NSFG_data$SECU)
  library(ggplot2)
  library(dplyr)
  library(splines)
  library(gridExtra)
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

cv.svy(HSNSFG, "income~race+YrEdu", nfolds = 4, nest = TRUE, weights = "wgt", N = 5100)
cv.svy(HSNSFG, "income~race+YrEdu", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = TRUE)

#this shows some difference but not a whole ton

cv.svy(HSNSFG, "income~race+BMI", nfolds = 4, nest = TRUE, weights = "wgt", N = 5100)
cv.svy(HSNSFG, "income~race+BMI", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = TRUE)
#again above were still within one SD

HSNSFG$PreMeInd <- 0
HSNSFG$PreMeInd[HSNSFG$PreMe == "Term"] <- 1
cv.svy(HSNSFG, "PreMeInd~race+income+Wanted", nfolds = 4, nest = TRUE, weights = "wgt", N = 5100, afolds = FALSE, method = "logistic")

cv.svy(HSNSFG, "PreMeInd~race+income+Wanted", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = TRUE, method = "logistic")

#here we are getting kinda close

cv.svy(HSNSFG, "PreMeInd~race+income+Wanted", nfolds = 4, weights = "wgt", N = 2500, afolds = FALSE, method = "logistic")

cv.svy(HSNSFG, "PreMeInd~race+income+Wanted", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 2500, afolds = TRUE, method = "logistic")

#couldn't really find anything here so i tried re grouping along some college

BSNSFG <-NSFG_data[NSFG_data$eduCat=="Bachelors",]


BSNSFG$PreMeInd <- 0
BSNSFG$PreMeInd[BNSFG$PreMe == "Term"] <- 1
cv.svy(BSNSFG, "income~race+YrEdu", nfolds = 4, nest = TRUE, weights = "wgt", N = 5100, afolds = FALSE)
cv.svy(BSNSFG, "income~race+YrEdu", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = TRUE)

#this shows some difference but not a whole ton

cv.svy(BSNSFG, "income~race+BMI", nfolds = 4, nest = TRUE, weights = "wgt", N = 5100, afolds = FALSE)
cv.svy(BSNSFG, "income~race+BMI", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = TRUE)
#maybe a bit more difference

cv.svy(BSNSFG, "PreMeInd~race+income+Wanted", nfolds = 4, nest = TRUE, weights = "wgt", N = 5100, afolds = FALSE, method = "logistic")

cv.svy(BSNSFG, "PreMeInd~race+income+Wanted", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = TRUE, method = "logistic")

#again not super close


#lets try a different breakdown
SCNSFG <-NSFG_data[NSFG_data$eduCat=="Some college",]

# not enough PSU


##cole found some examples that looked promising (see below) gonna write a plotting function for them

cv.svy(NSFG_data, c("income~ns(YrEdu, df = 1)","income~ns(YrEdu, df = 2)","income~ns(YrEdu, df = 3)","income~ns(YrEdu, df = 4)","income~ns(YrEdu, df = 5)"), nfolds = 4, weights = "wgt", N = 5100)
cv.svy(NSFG_data, c("income~ns(YrEdu, df = 1)","income~ns(YrEdu, df = 2)","income~ns(YrEdu, df = 3)","income~ns(YrEdu, df = 4)","income~ns(YrEdu, df = 5)"), nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100)

cv.svy(NSFG_data, "income~race", nfolds = 4, weights = "wgt", N = 5100)
cv.svy(NSFG_data, "income~race", nfolds = 4, strataID = "strata",
       clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100)

income.year_edu.plot <- function(loops) {
  # Making an empty data set for output when we take into consideration the design method
  method.ds <- data.frame(MSE = c())
  # Making an empty data set for output when we ignore the design method
  ignore.ds <- data.frame(MSE = c())
  #Making an empty dataset for output when we take into account everything except during fold generation
  method.fold.ds <- data.frame(MSE = c())
   # looping and generating MSEs
  for (i in 1:loops) {
    set.seed(i)
    method.data <- cv.svy(NSFG_data, c("income~ns(YrEdu, df = 1)","income~ns(YrEdu, df = 2)","income~ns(YrEdu, df = 3)","income~ns(YrEdu, df = 4)","income~ns(YrEdu, df = 5)","income~ns(YrEdu, df = 6)"), nfolds = 4, strataID = "strata",
                          clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100)
    method.ds2 <- data.frame(df = 1:6, MSE = method.data[,1])
    method.ds <- rbind(method.ds, method.ds2)
    ignore.data <- cv.svy(NSFG_data, c("income~ns(YrEdu, df = 1)","income~ns(YrEdu, df = 2)","income~ns(YrEdu, df = 3)","income~ns(YrEdu, df = 4)","income~ns(YrEdu, df = 5)","income~ns(YrEdu, df = 6)"), nfolds = 4, weights = "wgt", N = 5100)
    ignore.ds2 <- data.frame(df = 1:6, MSE = ignore.data[,1])
    ignore.ds <- rbind(ignore.ds, ignore.ds2)
    method.fold.data <- cv.svy(NSFG_data, c("income~ns(YrEdu, df = 1)","income~ns(YrEdu, df = 2)","income~ns(YrEdu, df = 3)","income~ns(YrEdu, df = 4)","income~ns(YrEdu, df = 5)","income~ns(YrEdu, df = 6)"), nfolds = 4, strataID = "strata",
                          clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100, afolds = FALSE)
    method.fold.ds2 <- data.frame(df = 1:6, MSE = method.fold.data[,1])
    method.fold.ds <- rbind(method.ds, method.ds2)
  }
  ignore.ds$df <- as.factor(ignore.ds$df)
  method.ds$df <- as.factor(method.ds$df)
  method.fold.ds$df <- as.factor(method.fold.ds$df)

  # Making a ggplot object for the MSE spread comparison
  plot1 <- ggplot(data = ignore.ds, mapping = aes(x = df, y = MSE)) +
    geom_boxplot() +
    ggtitle("Ignoring Design") +
    ylim(18000, 19000)
  plot2 <- ggplot(data = method.fold.ds, mapping = aes(x = df, y = MSE)) +
    geom_boxplot() +
    ggtitle("No Folds") +
    ylim(18000, 19000)
  plot3 <- ggplot(data = method.ds, mapping = aes(x = df, y = MSE)) +
    geom_boxplot() +
    ggtitle("Accounting for Design") +
    ylim(18000, 19000)

  # Making a grid display of the two plot objects above
  grid.arrange(plot1,plot2,plot3, ncol = 3)

}

income.race.plot <- function(loops) {
  # Making an empty data set for output when we take into consideration the design method
  method.ds <- data.frame(MSE = c())
  # Making an empty data set for output when we ignore the design method
  ignore.ds <- data.frame(MSE = c())
  # looping and generating MSEs
  for (i in 1:loops) {
    set.seed(i)
    method.data <- cv.svy(NSFG_data, "income~race", nfolds = 4, strataID = "strata",
                          clusterID = "SECU", nest = TRUE, weights = "wgt", N = 5100)
    method.ds2 <- data.frame(df = 5, MSE = method.data[,1])
    method.ds <- rbind(method.ds, method.ds2)
    ignore.data <- cv.svy(NSFG_data, "income~race", nfolds = 4, weights = "wgt", N = 5100)
    ignore.ds2 <- data.frame(df = 5, MSE = ignore.data[,1])
    ignore.ds <- rbind(ignore.ds, ignore.ds2)
  }
  ignore.ds$df <- as.factor(ignore.ds$df)
  method.ds$df <- as.factor(method.ds$df)

  plot1 <- ggplot(data = ignore.ds, mapping = aes(x = df, y = MSE)) +
    geom_boxplot() +
    ggtitle("Ignoring Design")
    #ylim(18000, 19000)
  # Making a ggplot object for the MSE spread comparison
  plot2 <- ggplot(data = method.ds, mapping = aes(x = df, y = MSE)) +
    geom_boxplot() +
    ggtitle("Account for Design")
    #ylim(18000, 19000)

  # Making a grid display of the two plot objects above
  grid.arrange(plot1,plot2, ncol = 2)

}
# we see a significant difference here
income.year_edu.plot(loops = 100)
#not so much here
income.race.plot(loops = 100)

#we should use the income~year_edu plot for our example in the vignette


}


