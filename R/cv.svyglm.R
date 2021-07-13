#' CV for `svyglm` objects
#'
#' Wrapper function which takes a `svyglm` object
#' (which itself contains a `svydesign` object)
#' and passes it through `cv.svydesign()` to `cv.svy()`.
#'
#' @param glm_object Name of a `svyglm` object created from the `survey` package
#' @param nfolds Number of folds to be used during cross validation, defaults to
#'   5
#' @param method String, must be either "linear" or "logistic", determines type of
#'   model fit during cross validation, defaults to linear
#' @examples
#' # MSEs generated for different tests of first and second degree polynomial
#' # fits predicting mpg from horsepower in the Auto dataset. Clustering and
#' # Stratification was done along the "year" variable.
#' # Using a survey glm to generate MSEs.
#' data("Auto", package = "ISLR")
#' library(survey)
#' auto.srs.svy <- svydesign(ids = ~0,
#'                          data = Auto)
#' srs.model <- svyglm(mpg~horsepower+I(horsepower^2)+I(horsepower^3),
#'                     design = auto.srs.svy)
#' auto.clus.svy <- svydesign(ids = ~year,
#'                           data = Auto)
#' clus.model <- svyglm(mpg~horsepower+I(horsepower^2)+I(horsepower^3),
#'                      design = auto.clus.svy)
#' auto.strat.svy <- svydesign(ids = ~0,
#'                            strata = ~year,
#'                            data = Auto)
#' strat.model <- svyglm(mpg~horsepower+I(horsepower^2)+I(horsepower^3),
#'                       design = auto.strat.svy)
#' cv.svyglm(glm_object = srs.model, nfolds = 10)
#' cv.svyglm(glm_object = clus.model, nfolds = 10)
#' cv.svyglm(glm_object = strat.model, nfolds = 10)
#' @export

# TODO: Write formal unit tests

cv.svyglm <- function(glm_object, nfolds = 5, method = c("linear", "logistic")) {

  method <- match.arg(method)

  formulae <- deparse1(glm_object[["formula"]])
  design_object <- glm_object[["survey.design"]]

  # Runs our cv.svydesign() function using the pieces pulled from the glm object,
  # which will later push all of this information into our general cv.svy() function.
  cv.svydesign(design_object = design_object, formulae = formulae,
               nfolds = nfolds, method = method)

}

