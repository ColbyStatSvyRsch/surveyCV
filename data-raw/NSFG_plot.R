# Using the preprocessed `NSFG_data` dataset to remake plot for the Stat paper
library(survey)
library(surveyCV)
library(splines)
library(magrittr)
library(ggplot2)
library(gridExtra)
data("NSFG_data")

# These data were preprocessed following Hunter Ratliff's code on RPubs here:
# https://rpubs.com/HunterRatliff1/NSFG_Wrangle
# stopping at the end of creating/editing the `data0` dataset.
#
# However, this data is at the PREGNANCY level --
#   each row is one pregnancy, so some mothers are in the dataset many times.
# Our analysis below should be at the RESPONDENT level,
# so subset to just one row per CASEID:
sub <- NSFG_data[!duplicated(NSFG_data$CASEID), ]
nrow(sub) == length(unique(NSFG_data$CASEID))
head(sub)
# OK, this looks right.
# Overwrite full dataframe with this subset
NSFG_data <- sub

# As a reasonable pair of variables to regress with the NSFG,
#   let's use income (at time of svy)
#   vs age (at time of 1st live birth's conception).
# The only other variable that's ALL about 1st birth is
#   `GA` = gestational age in weeks
#   but plotting age vs GA or vice versa shows no pattern,
#   and all CV methods agree that df=0 is best fit,
#   which is not so interesting to show in our paper.



# Models: Age (at time of conception of 1st live birth!) vs Income (at time of survey!)
modelsToFit <- c("income~ns(age, df = 1)","income~ns(age, df = 2)",
                 "income~ns(age, df = 3)","income~ns(age, df = 4)",
                 "income~ns(age, df = 5)","income~ns(age, df = 6)")

NSFG_data$strata <- as.factor(NSFG_data$strata)
NSFG_data$SECU <- as.factor(NSFG_data$SECU)

# # Because there are exactly 4 clusters in each stratum,
# # we can only do 4-fold CV, not 5-fold like for other sims/examples:
# data.frame(NSFG_data$strat, NSFG_data$SECU) %>%
#   unique() %>%
#   `[[`("NSFG_data.strat") %>%
#   table()





set.seed(47)


# Use clusters and strata for CV folds,
# and use weights to train and test models

method.data <- replicate(5, {
  cv.svy(NSFG_data, modelsToFit,
                      nfolds = 4, strataID = "strata",
                      clusterID = "SECU", nest = TRUE, weightsID = "wgt") %>% as.vector()
})
method.ds <- data.frame(df = 1:6, MSE = rowMeans(method.data),
                        type = "Weighted Survey CV")

# Use weights, but make SRS folds
nofold.data <- replicate(5, {
  cv.svy(NSFG_data, modelsToFit,
                    nfolds = 4, strataID = "strata",
                    clusterID = "SECU", nest = TRUE, weightsID = "wgt",
                    useSvyForFolds = FALSE) %>% as.vector()
})
nofold.ds <- data.frame(df = 1:6, MSE = rowMeans(nofold.data),
                        type = "Weighted SRS CV")

# Ignore survey entirely -- no weights, SRS folds
ignore.data <- replicate(5, {
  cv.svy(NSFG_data, modelsToFit,
                        nfolds = 4) %>% as.vector()
})
ignore.ds <- data.frame(df = 1:6, MSE = rowMeans(ignore.data),
                        type = "Unweighted SRS CV")

combined.ds <- rbind(method.ds, nofold.ds, ignore.ds)
combined.ds$type <- factor(combined.ds$type,
  c("Weighted Survey CV", "Weighted SRS CV", "Unweighted SRS CV"))




png("NSFG.png", 2000, 2000, pointsize = 16, res = 300)

yrange <- c(2.23, 2.27)
plot(1:6, ignore.ds$MSE/10000, type = 'b', col = "black",
     ylim = yrange, lty = 1, las = 1,
     ylab = "CV Test MSE / 10000", xlab = "df for spline model",
     main = paste0("NSFG data (5 reps of 4-fold CV)"))
lines(1:6, nofold.ds$MSE/10000, type = 'b', col = "red",
      ylim = yrange, lty = 2)
lines(1:6, method.ds$MSE/10000, type = 'b', col = "blue",
      ylim = yrange, lty = 3)
abline(v = 2.98,
       col = "black", lty = 1)
abline(v = 3,
       col = "red", lty = 2)
abline(v = 3.02,
       col = "blue", lty = 3)
legend("topright",
       c("Weighted Survey CV", "Weighted SRS CV",
         "Unweighted SRS CV"),
       col = c("blue", "red", "black"), lty = 3:1, pch = 21)

dev.off()
