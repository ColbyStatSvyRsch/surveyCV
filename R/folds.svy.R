#' Creating CV folds based on the survey design
#'
#' This function creates a fold ID for each row in the dataset,
#' to be used for carrying out cross validation on survey samples taken using a
#' SRS, stratified, clustered, or clustered-and-stratified sampling design.
#' Returns a vector of fold IDs, which in most cases you will want to append
#' to your dataset using \code{cbind} or similar (see Examples below).
#' These fold IDs respect any stratification or clustering in the survey design.
#' You can then carry out K-fold CV as usual,
#' taking care to also use the survey design features and survey weights
#' when fitting models in each training set
#' and also when evaluating models against each test set.
#' If you have already created a \code{svydesign} object,
#' you will probably prefer the convenience wrapper function
#' \code{\link{folds.svydesign}}.
#' For the special cases of linear or logistic GLMs, use instead
#' \code{\link{cv.svy}}, \code{\link{cv.svydesign}}, or \code{\link{cv.svyglm}}
#' which will automate the whole CV process for you.
#'
#' @param Data Dataframe of dataset
#' @param nfolds Number of folds to be used during cross validation
#' @param strataID String of the variable name used to stratify during sampling, must
#'   be the same as in the dataset used
#' @param clusterID String of the variable name used to cluster during sampling, must
#'   be the same as in the dataset used
#' @return Integer vector of fold IDs with length \code{nrow(Data)}.
#'   Most likely you will want to append the returned vector to your dataset,
#'   for instance with \code{cbind} (see Examples below).
#' @seealso \code{\link{folds.svydesign}} for a wrapper to use with a \code{svydesign} object
#' @seealso \code{\link{cv.svy}}, \code{\link{cv.svydesign}}, or \code{\link{cv.svyglm}}
#'   to carry out the whole CV process (not just forming folds but also training
#'   and testing your models) for linear or logistic regression models
#' @examples
#' # Set up CV folds for a stratified sample and a one-stage cluster sample,
#' # using data from the `survey` package
#' library(survey)
#' data("api", package = "survey")
#' # stratified sample
#' apistrat <- cbind(apistrat,
#'                   .foldID = folds.svy(apistrat, nfolds = 5, strataID = "stype"))
#' # Each fold will have observations from every stratum
#' with(apistrat, table(stype, .foldID))
#' # Fold sizes should be roughly equal
#' table(apistrat$.foldID)
#' #
#' # one-stage cluster sample
#' apiclus1 <- cbind(apiclus1,
#'                   .foldID = folds.svy(apiclus1, nfolds = 5, clusterID = "dnum"))
#' # For any given cluster, all its observations will be in the same fold;
#' # and each fold should contain roughly the same number of clusters
#' with(apiclus1, table(dnum, .foldID))
#' # But if cluster sizes are unequal,
#' # the number of individuals per fold will also vary
#' table(apiclus1$.foldID)
#' @export

# TODO: add an example using folds.svy() to carry out survey CV manually
# for some model (not linear or logistic) that surveyCV can't handle directly


# TODO: Consider combining all these 4 cases (SRS, Clus, Strat, and Clus+Strat)
#   into one Clus+Strat case where we just
#   set all cluster IDs to 1:n if there's no clustering, and/or
#   set all stratum IDs to 1 if there's no stratification

# TODO: Change the stopifnot() error message for strata:
#   if some strata are too small and you don't want to decrease nfolds,
#   suggest that the user could instead manually create larger pseudo-strata
#   by combining several smaller ones... e.g. see Lumley p43?

# TODO: Add a `verbose` argument, and if(verbose),
#   print() or cat() a report about the nr of samples and PSUs
#   in each fold or each strat-by-fold combo

# TODO: Maybe separate this out into appendfolds() vs makefolds(),
#   where appendfolds() is a wrapper (returns the dataset with .foldID in it),
#   but makefolds() returns ONLY .foldID, in case that's ever needed alone?
#   Perhaps may be useful for speeding up work with very large datasets?

# TODO: The way we generate folds for stratified samples seems OK,
#   but is there a better way?
#   Something to ensure table(strat|fold) is least variable?
#   (Would we even want that, if we could?)
#   Should we check Kuhn et al's rsample::vfold_cv(strata = ...) ???
#   https://rsample.tidymodels.org/reference/vfold_cv.html
#   https://github.com/tidymodels/rsample/blob/master/R/vfold.R
#   ...oh cool, they use enquo() and vars_select() to work with var names
#   directly instead of expecting them as strings...
#   maybe we should do this too, for specifying stratum / cluster variables etc?
#   https://tidyeval.tidyverse.org/dplyr.html
#   https://tidyselect.r-lib.org/reference/eval_select.html



# folds.svy()
# Inputs:
#   the dataset,
#   number of folds,
#   and strings for the names of the stratum and cluster variables in Data
#   (we don't need to ask whether nest=TRUE or FALSE because the code is written
#    to handle clusters separately within each stratum, if strata are given)
# Outputs:
#   .foldID = a vector of fold IDs, from 1 to nfolds,
#      in the order of original dataset's rows
#      (meant to be appended to it, e.g. as `Data <- cbind(Data, .foldID)`)
folds.svy <- function(Data, nfolds, strataID = NULL, clusterID = NULL) {
  stopifnot(nfolds <= nrow(Data))

  if(is.null(strataID) & is.null(clusterID)) {

    # SRS CV
    .foldID <- sample(rep(1:nfolds, length.out = nrow(Data)))

  } else if(is.null(strataID)) {

    # Clustered CV
    # Ignoring unequal cluster sizes for now,
    # just assign each cluster to a fold at random
    clus <- Data[[clusterID]]
    nClus <- length(unique(clus))
    stopifnot(nfolds <= nClus)
    foldID.clus <- sample(rep(1:nfolds, length.out = nClus))
    # Then assign these cluster-level foldIDs to the corresponding rows of data
    foldID.df <- merge(data.frame(id = 1:length(clus), clus = clus),
                       data.frame(clus = unique(clus), foldID = foldID.clus),
                       sort = FALSE)
    # Sort them to be in the dataset's original order
    foldID.df <- foldID.df[order(foldID.df$id), ]
    .foldID <- foldID.df$foldID

  } else if(is.null(clusterID)) {

    # Stratified CV
    strat <- Data[[strataID]]
    n <- nrow(Data)
    # Each fold should be able to have at least one obs from each stratum
    stopifnot(nfolds <= min(table(strat)))

    # Scramble, then reorder by stratum
    foldID.df <- data.frame(id = 1:n, strat = strat)
    foldID.df <- foldID.df[sample(1:n), ]
    foldID.df <- foldID.df[order(foldID.df$strat), ]
    # Assign fold IDs sequentially -- now folds will be balanced across strata,
    # but random within each stratum
    foldID.df$foldID <- rep(1:nfolds, length.out = n)
    # Sort them to be in the dataset's original order
    foldID.df <- foldID.df[order(foldID.df$id), ]
    .foldID <- foldID.df$foldID

  } else {

    # CV with both strata and clusters

    # TODO: (Could we make this function even more modular?
    # Make separate appendfolds functions for strat vs clus vs SRS,
    # and then just have the strat function
    # call either the clus or SRS function with each stratum?
    # ...
    # Hmmm -- perhaps not -- at least the simple/naive approach would lead to
    # having more cases in fold 1 than in fold K in *each* stratum
    # when strata sizes don't divide nfolds equally,
    # and that becomes even worse when we combine strata.)
    # So...
    # if we *don't* re-use the same function independently in each stratum,
    # can we do better at balancing folds vs strata?
    # ...
    # Sort by strata, then shuffle clusters within each stratum,
    # then assign folds sequentially to each strat.clus combo.
    # Then merge those folds back in with row IDs, and finally sort on row ID.
    # (This should work whether or not nest=TRUE,
    # i.e. whether or not clusters in different strata have different names.)

    strat <- Data[[strataID]]
    clus <- Data[[clusterID]]
    n <- nrow(Data)

    # Get all unique stratum-cluster combos
    foldID.clus <- unique(data.frame(strat, clus))
    # Each fold should be able to have at least one cluster from each stratum
    stopifnot(nfolds <= table(foldID.clus$strat))

    # Then do as for stratified CV, but at the cluster level first:
    # Scramble, then reorder by stratum
    foldID.clus <- foldID.clus[sample(1:nrow(foldID.clus)), ]
    foldID.clus <- foldID.clus[order(foldID.clus$strat), ]
    # Assign fold IDs sequentially -- now folds will be balanced across strata,
    # but random within each stratum
    foldID.clus$foldID <- rep(1:nfolds, length.out = nrow(foldID.clus))

    # Then assign these cluster-level foldIDs to the corresponding rows of data
    foldID.df <- merge(data.frame(id = 1:n, strat = strat, clus = clus),
                       foldID.clus)

    # Sort them to be in the dataset's original order
    foldID.df <- foldID.df[order(foldID.df$id), ]
    .foldID <- foldID.df$foldID

  }

  return(.foldID)
}

