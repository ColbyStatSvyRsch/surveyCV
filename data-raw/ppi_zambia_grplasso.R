# R code used to generate Figure 1a in the Stat paper
# submitted for the 2021 SDSS special issue.
# We cannot share the proprietary dataset used here, only the code.

set.seed(12345)

nfolds <- 5
nreps <- 5 # repeat K-fold CV this many times

rerunSims <- FALSE
resavePlot <- TRUE
subsampleData <- TRUE


library(foreign)
library(grplasso)
# What group-lasso package to use?
# -- grplasso does not build in CV, but allows weights,
#    so we could run our own CV code in a loop around it. USE THIS ONE.
# -- grpreg has built-in CV which allows you to specify your own folds,
#      but does not allow observation weights.
# -- gglasso has both specify-your-own-folds CV and weights,
#      but they don't work together.
# -- glinternet doesn't allow control of either.
# -- glmnet has both, but not group lasso.



# Read in the data
survey_data  <- read.dta("data.dta", convert.factors = FALSE)
question_key <- read.csv("question_key.csv")

# Subset to just the response variable "poorl" and the predictors listed in question_key,
# which are the "nice" predictors that have been determined by subject matter experts
# to be feasible for use in a scorecard.
data4model <- survey_data[c("poorl", as.character(question_key$variable_name))]
head(data4model)
# Also use the weight variable that was used in our PPI code for NeurIPS paper,
# and the apparent cluster variable
cluster4model <- survey_data$cluster
weight4model  <- survey_data$hh_weight



# SUBSET down to just a small fraction of the original *clusters*,
# mimicking the subsampling used during intermediate steps in PPI development
if(subsampleData) {
  # RANDOMLY SUBSAMPLE CLUSTERS
  clusIDs <- unique(cluster4model)
  whichClus <- which(cluster4model %in% sample(clusIDs, 30))

  data4model <- data4model[whichClus, ]
  cluster4model <- cluster4model[whichClus]
  weight4model <- weight4model[whichClus]
  nrow(data4model)
}
# Create a rescaled weight variable with avg weight 1,
# which should ensure that wtd/unwtd lasso
# can be meaningfully compared on the same lambda scales
weight4model.std <- nrow(data4model) * weight4model/sum(weight4model)






# Create a vector of group assignments,
# with all the regions set to group NA so they are unpenalized...
group <- question_key$unique_q_number
group[54:62] <- NA
# Double-check that the right sets of variables are assigned to groups together
data.frame(varname = names(data4model), grp = c(0, group))

# AND ADD INTERCEPT as the last column of x-matrix
# (since the totals for the regions DON'T add up to sample size,
#  there must have been a baseline region left out, so we do need intercept,
#  which `grplasso()` does NOT automatically include)
group <- c(group, NA)
X <- cbind(as.matrix(data4model[, -1]), intercept = 1)
data.frame(varname = colnames(X), grp = group)

# Create a lambda grid manually
lambdas <- 2^seq(from = 5, to = -2, by = -0.5)






# CROSS VALIDATE

if(rerunSims){

  loss.SRS.NoWt <- loss.SRS.Wt <- loss.clus.Wt <- rep(0, length(lambdas))
  for(rr in 1:nreps) {

    # MAKE NEW FOLDS
    folds.df <- data.frame(ID = 1:nrow(data4model),
                           clus = cluster4model)
    folds.df <- surveyCV:::appendfolds(folds.df, nfolds = nfolds)
    folds.df$SRSfoldID <- folds.df$.foldID
    folds.df$.foldID <- NULL
    folds.df <- surveyCV:::appendfolds(folds.df, nfolds = nfolds,
                                       clusterID = "clus")
    folds.df$clusfoldID <- folds.df$.foldID
    folds.df$.foldID <- NULL
    head(folds.df)
    with(folds.df, table(clus, clusfoldID))
    with(folds.df, table(clus, SRSfoldID))
    # Good - each cluster has one and only one clusfoldID, but many SRSfoldIDs


    # SRS CV WITHOUT WEIGHTS
    lossByLambda.sum <- rep(0, length(lambdas))
    for(ff in 1:nfolds) {
      trainIDs <- which(!folds.df$SRSfoldID %in% ff)
      fit.train <- suppressWarnings(
        grplasso(x = X[trainIDs,], y = data4model$poorl[trainIDs],
                            index = group, # no wts
                            model = LogReg(),
                            lambda = lambdas,
                            center = FALSE, standardize = FALSE))
      yhat.test <- predict(fit.train, newdata = X[-trainIDs, ], type = "response")
      y.test <- data4model$poorl[-trainIDs]
      loss <- -(y.test * log(yhat.test) + (1-y.test) * log(1-yhat.test))
      lossByLambda.sum <- lossByLambda.sum + colSums(loss)
    }
    lossByLambda.mean.SRS.NoWt <- lossByLambda.sum/nrow(data4model)
    plot(-log(lambdas), lossByLambda.mean.SRS.NoWt, type = 'b', col = "black",
         main = paste("Rep", rr))


    # SRS CV WITH WEIGHTS
    lossByLambda.sum <- rep(0, length(lambdas))
    for(ff in 1:nfolds) {
      trainIDs <- which(!folds.df$SRSfoldID %in% ff)
      fit.train.Wt <- suppressWarnings(
        grplasso(x = X[trainIDs,], y = data4model$poorl[trainIDs],
                               index = group, weights = weight4model.std[trainIDs],
                               model = LogReg(),
                               lambda = lambdas,
                               center = FALSE, standardize = FALSE))
      yhat.test.Wt <- predict(fit.train.Wt, newdata = X[-trainIDs, ], type = "response")
      y.test <- data4model$poorl[-trainIDs]
      loss.Wt <- -(y.test * log(yhat.test.Wt) + (1-y.test) * log(1-yhat.test.Wt))
      colSum.loss.Wt <- apply(loss.Wt, 2,
        function(x) sum(x * weight4model.std[-trainIDs]))
      lossByLambda.sum <- lossByLambda.sum + colSum.loss.Wt
    }
    lossByLambda.mean.SRS.Wt <- lossByLambda.sum/sum(weight4model.std)
    plot(-log(lambdas), lossByLambda.mean.SRS.Wt, type = 'b', col = "red",
         main = paste("Rep", rr))


    # CLUSTER CV WITH WEIGHTS
    lossByLambda.sum <- rep(0, length(lambdas))
    for(ff in 1:nfolds) {
      trainIDs <- which(!folds.df$clusfoldID %in% ff)
      fit.train.Wt <- suppressWarnings(
        grplasso(x = X[trainIDs,], y = data4model$poorl[trainIDs],
                               index = group, weights = weight4model.std[trainIDs],
                               model = LogReg(),
                               lambda = lambdas,
                               center = FALSE, standardize = FALSE))
      yhat.test.Wt <- predict(fit.train.Wt, newdata = X[-trainIDs, ], type = "response")
      y.test <- data4model$poorl[-trainIDs]
      loss.Wt <- -(y.test * log(yhat.test.Wt) + (1-y.test) * log(1-yhat.test.Wt))
      colSum.loss.Wt <- apply(loss.Wt, 2,
        function(x) sum(x * weight4model.std[-trainIDs]))
      lossByLambda.sum <- lossByLambda.sum + colSum.loss.Wt
    }
    lossByLambda.mean.clus.Wt <- lossByLambda.sum/sum(weight4model.std)
    plot(-log(lambdas), lossByLambda.mean.clus.Wt, type = 'b', col = "blue",
         main = paste("Rep", rr))

    loss.SRS.NoWt <- loss.SRS.NoWt + lossByLambda.mean.SRS.NoWt
    loss.SRS.Wt <- loss.SRS.Wt + lossByLambda.mean.SRS.Wt
    loss.clus.Wt <- loss.clus.Wt + lossByLambda.mean.clus.Wt
  }
  loss.SRS.NoWt <- loss.SRS.NoWt/nreps
  loss.SRS.Wt <- loss.SRS.Wt/nreps
  loss.clus.Wt <- loss.clus.Wt/nreps

} # end of if(rerunSims)



if(rerunSims) {
  save(loss.SRS.NoWt, loss.SRS.Wt, loss.clus.Wt,
       nfolds, nreps,
       file = "pp_zambia_grplasso.Rda")
} else {
  load("pp_zambia_grplasso.Rda")
}


if(resavePlot){
  png("Ppi_GrpLasso.png", 2000, 2000, pointsize = 16, res = 300)
}


# Find out how many coefs were included in each model
# if we use the chosen lambda and refit to full dataset

# UNWTD for lambdas[which.min(loss.SRS.NoWt)]
fit <- grplasso(x = X, y = data4model$poorl,
                index = group, # no wts
                model = LogReg(),
                lambda = lambdas[which.min(loss.SRS.NoWt)],
                center = FALSE, standardize = FALSE)
nrvar.SRS.NoWt <- sum(coef(fit) != 0)

# WTD for the other two
fit <- grplasso(x = X, y = data4model$poorl,
                index = group, weights = weight4model.std,
                model = LogReg(),
                lambda = lambdas[c(which.min(loss.SRS.Wt),
                                   which.min(loss.clus.Wt))],
                center = FALSE, standardize = FALSE)
nrvar.SRS.Wt <- sum(coef(fit)[,1] != 0)
nrvar.clus.Wt <- sum(coef(fit)[,2] != 0)


ymin <- min(loss.SRS.NoWt, loss.SRS.Wt, loss.clus.Wt)
ymax <- max(loss.SRS.NoWt, loss.SRS.Wt, loss.clus.Wt)
plot(-log(lambdas), loss.SRS.NoWt, type = 'b', col = "black",
     ylim = c(ymin, ymax), lty = 1, las = 1,
     ylab = "CV Test Logistic Loss", xlab = "-log(lambda) for logistic lasso model",
     main = paste0("PPI data (", nreps, " reps of ", nfolds, "-fold CV)"))
lines(-log(lambdas), loss.SRS.Wt, type = 'b', col = "red",
      ylim = c(ymin, ymax), lty = 2)
lines(-log(lambdas), loss.clus.Wt, type = 'b', col = "blue",
      ylim = c(ymin, ymax), lty = 3)
abline(v = -log(lambdas[which.min(loss.SRS.NoWt)]),
       col = "black", lty = 1)
abline(v = -log(lambdas[which.min(loss.SRS.Wt)]),
       col = "red", lty = 2)
abline(v = -log(lambdas[which.min(loss.clus.Wt)]),
       col = "blue", lty = 3)
legend("topleft",
       c(paste("Weighted Survey CV:", nrvar.clus.Wt, "vars"),
         paste("Weighted SRS CV: ", nrvar.SRS.Wt, "vars"),
         paste("Unweighted SRS CV: ", nrvar.SRS.NoWt, "vars")),
       col = c("blue", "red", "black"), lty = 3:1, pch = 21)

if(resavePlot){
  dev.off()
}

