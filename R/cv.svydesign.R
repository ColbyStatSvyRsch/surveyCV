#' CV for \code{svydesign} objects
#'
#' Wrapper function which takes a \code{\link[survey]{svydesign}} object
#' and a vector of model formulas (as strings),
#' and passes it into \code{\link{cv.svy}}.
#' Returns survey CV estimates of the mean loss for each model
#' (MSE for linear models, or binary cross-entropy for logistic models).
#'
#' If you have already fitted a \code{svyglm},
#' you may prefer the convenience wrapper function
#' \code{\link{cv.svyglm}}.
#'
#' For models other than linear or logistic regression,
#' you can use \code{\link{folds.svy}} or \code{\link{folds.svydesign}} to generate
#' CV fold IDs that respect any stratification or clustering in the survey design.
#' You can then carry out K-fold CV as usual,
#' taking care to also use the survey design features and survey weights
#' when fitting models in each training set
#' and also when evaluating models against each test set.
#'
#' @param design_object Name of a \code{svydesign} object created using the \code{survey}
#'   package. We do not yet support use of replicate designs.
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

  method <- match.arg(method)

  # Check whether the input is the appropriate type of object
  # Note that replicate designs do not have the class 'survey.design'
  if (inherits(design_object, "svyrep.design")) {
    stop("Replicate designs are not currently supported.")
  }
  if (!inherits(design_object, "survey.design")) {
    stop("`design_object` must be a survey design object created with the 'survey' or 'srvyr' packages.")
  }

  if (ncol(design_object[['cluster']]) > 1) {
    warning("Only first-stage clusters and strata will be used.")
  }

  # Extract the variables and design information from the design object
  # Special handling for the variables is needed for database-backed designs
  if (inherits(design_object, "DBIsvydesign")) {
    formula_vars <- lapply(formulae, function(formula_string) all.vars(as.formula(formula_string)))
    formula_vars <- Reduce(x = formula_vars, f = unique)
    getvars_fn <- utils::getFromNamespace(x = "getvars", ns = "survey")
    .data <- getvars_fn(formula = formula_vars,
                        dbconnection = design_object$db$connection,
                        tables = design_object$db$tablename,
                        updates = design_object$updates,
                        subset = design_object$subset)
  } else {
    .data <- design_object[["variables"]]
  }

  .data[['DESIGN__PSU__']] <- design_object[['cluster']][[1]]
  .data[['DESIGN__FIRST_STAGE_STRATUM__']] <- design_object[['strata']][[1]]
  .data[['DESIGN__WEIGHTS__']] <- 1/design_object$prob
  if (!is.null(design_object$fpc$popsize)) {
    .data[['DESIGN__N_POP_PSUS_IN_STRATUM__']] <- design_object$fpc$popsize[,1, drop = TRUE]
    fpcID <- 'DESIGN__N_POP_PSUS_IN_STRATUM__'
  } else {
    fpcID <- NULL
  }

  # Runs our general cv.svy() function with all of the variables and data pulled from the design object
  cv.svy(Data = .data, formulae = formulae, nfolds = nfolds,
         clusterID = "DESIGN__PSU__", strataID = "DESIGN__FIRST_STAGE_STRATUM__", nest = FALSE,
         weightsID = "DESIGN__WEIGHTS__",
         fpcID = fpcID, method = method,
         na.rm = na.rm)

}

