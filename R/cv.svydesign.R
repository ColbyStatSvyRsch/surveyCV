#' CV for `svydesign` objects
#'
#' Wrapper function which takes a `svydesign` object
#' and a vector of model formulas (as strings),
#' and passes it into `cv.svy()`.
#'
#' @param design_object Name of a `svydesign` object created using the `survey`
#'   package. The argument `ids` (also `strata`, `fpc`, and/or `weights` if used)
#'   must be specified as formulas, e.g. `svydesign(ids = ~MyPSUs, ...)`.
#'   We do not yet support use of `probs` or `pps`.
#' @param formulae Vector of formulas (as strings) for the GLMs to be compared in
#'   cross validation
#' @param nfolds Number of folds to be used during cross validation, defaults to
#'   5
#' @param method String, must be either "linear" or "logistic", determines type of
#'   model fit during cross validation, defaults to linear
#' @examples
#' # MSEs generated for different tests of first and second degree polynomial
#' # fits predicting mpg from horsepower in the Auto dataset. Clustering and
#' # Stratification was done along the "year" variable.
#' # Using a survey design object to generate MSEs.
#' data("Auto", package = "ISLR")
#' library(survey)
#' auto.srs.svy <- svydesign(ids = ~0,
#'                          data = Auto)
#' auto.clus.svy <- svydesign(ids = ~year,
#'                           data = Auto)
#' auto.strat.svy <- svydesign(ids = ~0,
#'                            strata = ~year,
#'                            data = Auto)
#' cv.svydesign(formulae = c("mpg~poly(horsepower,1, raw = TRUE)",
#'                           "mpg~poly(horsepower,2, raw = TRUE)",
#'                           "mpg~poly(horsepower,3, raw = TRUE)"),
#'              design_object = auto.srs.svy, nfolds = 10)
#' cv.svydesign(formulae = c("mpg~poly(horsepower,1, raw = TRUE)",
#'                           "mpg~poly(horsepower,2, raw = TRUE)",
#'                           "mpg~poly(horsepower,3, raw = TRUE)"),
#'              design_object = auto.clus.svy, nfolds = 10)
#' cv.svydesign(formulae = c("mpg~poly(horsepower,1, raw = TRUE)",
#'                           "mpg~poly(horsepower,2, raw = TRUE)",
#'                           "mpg~poly(horsepower,3, raw = TRUE)"),
#'              design_object = auto.strat.svy, nfolds = 10)
#' @export


# TODO: Write formal unit tests



cv.svydesign <- function(design_object, formulae, nfolds = 5, method = c("linear", "logistic")) {
  # When a survey design object is specified,
  # then the function can pull pieces of information needed from the design object

  method <- match.arg(method)

  # Extract the dataframe from the design object
  .data <- design_object[["variables"]]

  # Create a clusterID variable from the design object
  ids.formula <- design_object[["call"]][["ids"]]
  stopifnot(length(ids.formula) == 2)
  clusterID = as.character(ids.formula[[2]])
  if (clusterID %in% c("0", "1")) {
    clusterID = NULL
  }

  # Create a strataID variable if it is present in the design object
  if (design_object[["has.strata"]] == TRUE) {
    strata.formula <- design_object[["call"]][["strata"]]
    stopifnot(length(strata.formula) == 2)
    strataID = as.character(strata.formula[[2]])
  } else {
    strataID = NULL
  }

  # Makes a nest = TRUE for later use if needed
  if (is.null(design_object[["call"]][["nest"]])) {
    nest = FALSE
  } else if (design_object[["call"]][["nest"]] == TRUE) {
    nest = TRUE
  } else {
    nest = FALSE
  }

  # Makes a weights variable if present in the design object
  if ( !is.null(design_object[["call"]][["probs"]]) ) {
    stop("`svydesign(probs=...) is not supported yet. Please specify weights instead.")
  }
  weights.formula <- design_object[["call"]][["weights"]]
  if ( !is.null(weights.formula) ) {
    stopifnot(length(weights.formula) == 2)
    weightsID = as.character(weights.formula[[2]])
  } else {
    weightsID = NULL
  }

  # Makes a fpc variable if present in the design object
  fpc.formula <- design_object[["call"]][["fpc"]]
  if ( !is.null(fpc.formula) ) {
    stopifnot(length(fpc.formula) == 2)
    fpcID = as.character(fpc.formula[[2]])
  } else {
    fpcID = NULL
  }


  # Runs our general cv.svy() function with all of the variables and data pulled from the design object
  cv.svy(Data = .data, formulae = formulae, nfolds = nfolds,
         clusterID = clusterID, strataID = strataID, nest = nest,
         fpcID = fpcID, method = method,
         weightsID = weightsID)

}

