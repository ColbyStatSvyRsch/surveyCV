#' CV for \code{svydesign} objects
#'
#' Wrapper function which takes a \code{\link[survey]{svydesign}} object
#' and a vector of model formulas (as strings),
#' and passes it into \code{\link{cv.svy}}.
#' Returns survey CV estimates of the mean loss for each model
#' (MSE for linear models, or binary cross-entropy for logistic models).
#' If you have already fitted a \code{svyglm},
#' you may prefer the convenience wrapper function
#' \code{\link{cv.svyglm}}.

#'
#' @param design_object Name of a \code{svydesign} object created using the \code{survey}
#'   package. The argument \code{id} (also \code{strata}, \code{fpc}, and/or \code{weights} if used)
#'   must be specified as formulas, e.g. \code{svydesign(ids = ~MyPSUs, ...)}.
#'   We do not yet support use of \code{probs} or \code{pps}.
#' @param formulae Vector of formulas (as strings) for the GLMs to be compared in
#'   cross validation
#' @param nfolds Number of folds to be used during cross validation, defaults to
#'   5
#' @param method String, must be either "linear" or "logistic", determines type of
#'   model fit during cross validation, defaults to linear
#' @param na.rm Whether to drop cases with missing values when taking `svymean`
#'   of test losses
#' @return Object of class \code{svystat}, which is a named vector of survey CV estimates of the mean loss
#'   (MSE for linear models, or binary cross-entropy for logistic models) for each model,
#'   with names ".Model_1", ".Model_2", etc. corresponding to the models provided in \code{formulae};
#'   and with a \code{var} attribute giving the variances.
#'   See \code{\link[survey]{surveysummary}} for details.
#' @seealso \code{\link[survey]{surveysummary}}, \code{\link[survey]{svydesign}}
#' @seealso \code{\link{cv.svyglm}} for a wrapper to use with a \code{svyglm} object
#' @examples
#' # Compare CV MSEs and their SEs under 3 linear models
#' # for a stratified sample and a one-stage cluster sample,
#' # using data from the `survey` package
#' library(survey)
#' data("api", package = "survey")
#' # stratified sample
#' dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat,
#'                     fpc = ~fpc)
#' cv.svydesign(formulae = c("api00~ell",
#'                           "api00~ell+meals",
#'                           "api00~ell+meals+mobility"),
#'              design_object = dstrat, nfolds = 5)
#' # one-stage cluster sample
#' dclus1 <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
#' cv.svydesign(formulae = c("api00~ell",
#'                           "api00~ell+meals",
#'                           "api00~ell+meals+mobility"),
#'              design_object = dclus1, nfolds = 5)
#'
#' # Compare CV MSEs and their SEs under 3 linear models
#' # for a stratified cluster sample with clusters nested within strata
#' data(NSFG_data)
#' library(splines)
#' NSFG.svydes <- svydesign(id = ~SECU, strata = ~strata, nest = TRUE,
#'                          weights = ~wgt, data = NSFG_data)
#' cv.svydesign(formulae = c("income ~ ns(age, df = 2)",
#'                           "income ~ ns(age, df = 3)",
#'                           "income ~ ns(age, df = 4)"),
#'              design_object = NSFG.svydes, nfolds = 4)
#'
#' # Logistic regression example, using the same stratified cluster sample;
#' # instead of CV MSE, we calculate CV binary cross-entropy loss,
#' # where (as with MSE) lower values indicate better fitting models
#' # (NOTE: na.rm=TRUE is not usually ideal;
#' #  it's used below purely for convenience, to keep the example short,
#' #  but a thorough analysis would look for better ways to handle the missing data)
#' cv.svydesign(formulae = c("KnowPreg ~ ns(age, df = 1)",
#'                           "KnowPreg ~ ns(age, df = 2)",
#'                           "KnowPreg ~ ns(age, df = 3)"),
#'              design_object = NSFG.svydes, nfolds = 4,
#'              method = "logistic", na.rm = TRUE)
#' @export


# TODO: Write formal unit tests



cv.svydesign <- function(design_object, formulae, nfolds = 5,
                         method = c("linear", "logistic"), na.rm = FALSE) {
  # When a survey design object is specified,
  # then the function can pull pieces of information needed from the design object

  method <- match.arg(method)

  # Extract the dataframe from the design object
  .data <- design_object[["variables"]]

  # Create a clusterID variable from the design object
  ids.formula <- design_object[["call"]]$id
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
         weightsID = weightsID,
         na.rm = na.rm)

}

