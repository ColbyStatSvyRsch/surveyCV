% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/folds.svy.R
\name{folds.svy}
\alias{folds.svy}
\title{Creating CV folds based on the survey design}
\usage{
folds.svy(Data, nfolds, strataID = NULL, clusterID = NULL)
}
\arguments{
\item{Data}{Dataframe of dataset}

\item{nfolds}{Number of folds to be used during cross validation}

\item{strataID}{String of the variable name used to stratify during sampling, must
be the same as in the dataset used}

\item{clusterID}{String of the variable name used to cluster during sampling, must
be the same as in the dataset used}
}
\value{
Integer vector of fold IDs with length \code{nrow(Data)}.
  Most likely you will want to append the returned vector to your dataset,
  for instance with \code{cbind} (see Examples below).
}
\description{
This function creates a fold ID for each row in the dataset,
to be used for carrying out cross validation on survey samples taken using a
SRS, stratified, clustered, or clustered-and-stratified sampling design.
Returns a vector of fold IDs, which in most cases you will want to append
to your dataset using \code{cbind} or similar (see Examples below).
These fold IDs respect any stratification or clustering in the survey design.
You can then carry out K-fold CV as usual,
taking care to also use the survey design features and survey weights
when fitting models in each training set
and also when evaluating models against each test set.
}
\details{
If you have already created a \code{svydesign} object,
you will probably prefer the convenience wrapper function
\code{\link{folds.svydesign}}.

For the special cases of linear or logistic GLMs, use instead
\code{\link{cv.svy}}, \code{\link{cv.svydesign}}, or \code{\link{cv.svyglm}}
which will automate the whole CV process for you.
}
\examples{
# Set up CV folds for a stratified sample and a one-stage cluster sample,
# using data from the `survey` package
library(survey)
data("api", package = "survey")
# stratified sample
apistrat <- cbind(apistrat,
                  .foldID = folds.svy(apistrat, nfolds = 5, strataID = "stype"))
# Each fold will have observations from every stratum
with(apistrat, table(stype, .foldID))
# Fold sizes should be roughly equal
table(apistrat$.foldID)
#
# one-stage cluster sample
apiclus1 <- cbind(apiclus1,
                  .foldID = folds.svy(apiclus1, nfolds = 5, clusterID = "dnum"))
# For any given cluster, all its observations will be in the same fold;
# and each fold should contain roughly the same number of clusters
with(apiclus1, table(dnum, .foldID))
# But if cluster sizes are unequal,
# the number of individuals per fold will also vary
table(apiclus1$.foldID)
# See the end of `intro` vignette for an example of using such folds
# as part of a custom loop over CV folds
# to tune parameters in a design-consistent random forest model
}
\seealso{
\code{\link{folds.svydesign}} for a wrapper to use with a \code{svydesign} object

\code{\link{cv.svy}}, \code{\link{cv.svydesign}}, or \code{\link{cv.svyglm}}
  to carry out the whole CV process (not just forming folds but also training
  and testing your models) for linear or logistic regression models
}
