#' Creating CV folds based on the \code{svydesign} object
#'
#' Wrapper function which takes a \code{\link[survey]{svydesign}} object
#' and desired number of CV folds,
#' and passes it into \code{\link{folds.svy}}.
#' Returns a vector of fold IDs, which in most cases you will want to append
#' to your \code{svydesign} object using \code{update.svydesign}
#' (see Examples below).
#' These fold IDs respect any first-stage stratification or clustering in the survey design.
#' You can then carry out K-fold CV as usual,
#' taking care to also use the survey design features and survey weights
#' when fitting models in each training set
#' and also when evaluating models against each test set.
#'
#' For the special cases of linear or logistic GLMs, use instead
#' \code{\link{cv.svydesign}} or \code{\link{cv.svyglm}}
#' which will automate the whole CV process for you.
#'
#' @param design_object Name of a \code{svydesign} object created using the \code{survey}
#'   package. Replicate designs are not currently supported, nor is
#'   PPS sampling without replacement; and only first-stage
#'   stratification and clusters are used.
#' @param nfolds Number of folds to be used during cross validation
#' @return Integer vector of fold IDs with length \code{nrow(Data)}.
#'   Most likely you will want to append the returned vector
#'   to the \code{svydesign} object,
#'   for instance with \code{update.svydesign} (see Examples below).
#' @seealso \code{\link{folds.svy}}
#' @seealso \code{\link{cv.svy}}, \code{\link{cv.svydesign}}, or \code{\link{cv.svyglm}}
#'   to carry out the whole CV process (not just forming folds but also training
#'   and testing your models) for linear or logistic regression models
#' @examples
#' # Set up CV folds for a stratified sample and a one-stage cluster sample,
#' # using data from the `survey` package
#' library(survey)
#' data("api", package = "survey")
#' # stratified sample
#' dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat,
#'                     fpc = ~fpc)
#' dstrat <- update(dstrat, .foldID = folds.svydesign(dstrat, nfolds = 5))
#' # Each fold will have observations from every stratum
#' with(dstrat$variables, table(stype, .foldID))
#' # Fold sizes should be roughly equal
#' table(dstrat$variables$.foldID)
#' #
#' # one-stage cluster sample
#' dclus1 <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
#' dclus1 <- update(dclus1, .foldID = folds.svydesign(dclus1, nfolds = 5))
#' # For any given cluster, all its observations will be in the same fold;
#' # and each fold should contain roughly the same number of clusters
#' with(dclus1$variables, table(dnum, .foldID))
#' # But if cluster sizes are unequal,
#' # the number of individuals per fold will also vary
#' table(dclus1$variables$.foldID)
#' # See the end of `intro` vignette for an example of using such folds
#' # as part of a custom loop over CV folds
#' # to tune parameters in a design-consistent random forest model
#' @export

# TODO: add an example using folds.svydesign() to carry out survey CV manually
# for some model (not linear or logistic) that surveyCV can't handle directly



# folds.svydesign()
# Inputs:
#   the svydesign object,
#   and number of folds
# Outputs:
#   .foldID = a vector of fold IDs, from 1 to nfolds,
#      in the order of original dataset's rows
#      (meant to be appended to it, e.g. as `Data <- cbind(Data, .foldID)`)
folds.svydesign <- function(design_object, nfolds) {

  # Check whether the input is the appropriate type of object
  # Note that replicate designs do not have the class 'survey.design'
  if (inherits(design_object, "svyrep.design")) {
    stop("Replicate designs are not currently supported.")
  }
  if (!inherits(design_object, "survey.design")) {
    stop("`design_object` must be a survey design object created with the 'survey' or 'srvyr' packages.")
  }

  if (ncol(design_object[['cluster']]) > 1 |
      ncol(design_object[['strata']])  > 1) {
    warning("Only first-stage clusters and strata will be used.")
  }

  # Create clusterID and strataID variables from the design object
  # using only the first stage of sampling
    .data <- data.frame(
      'PSU' = design_object[['cluster']][[1]],
      'FIRST_STAGE_STRATUM' = design_object[['strata']][[1]]
    )


  # Runs our general folds.svy() function
  # with all of the variables and data pulled from the design object
  return(.foldID = folds.svy(Data = .data, nfolds = nfolds,
                             strataID = "FIRST_STAGE_STRATUM", clusterID = "PSU"))
}

