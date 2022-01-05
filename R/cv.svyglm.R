#' CV for `svyglm` objects
#'
#' Wrapper function which takes a `svyglm` object
#' (which itself contains a `svydesign` object)
#' and passes it through `cv.svydesign()` to `cv.svy()`.
#' Chooses linear or logistic regression based on the `svyglm` object's family.
#'
#' @param glm_object Name of a `svyglm` object created from the `survey` package
#' @param nfolds Number of folds to be used during cross validation, defaults to
#'   5
#' @examples
#' # Calculate CV MSE and its SE under one `svyglm` model
#' # for a stratified sample and a one-stage cluster sample,
#' # from the `survey` package
#' data("api", package = "survey")
#' # stratified sample
#' dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat,
#'                     fpc = ~fpc)
#' glmstrat <- svyglm(api00 ~ ell+meals+mobility, design = dstrat)
#' cv.svyglm(glmstrat, nfolds = 5)
#' # one-stage cluster sample
#' dclus1 <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
#' glmclus1 <- svyglm(api00 ~ ell+meals+mobility, design = dclus1)
#' cv.svyglm(glmclus1, nfolds = 5)
#'
#' # Calculate CV MSE and its SE under one `svyglm` model
#' # for a stratified cluster sample with clusters nested within strata
#' data(NSFG_data)
#' library(splines)
#' NSFG.svydes <- svydesign(id = ~SECU, strata = ~strata, nest = TRUE,
#'                          weights = ~wgt, data = NSFG_data)
#' NSFG.svyglm <- svyglm(income ~ ns(age, df = 3), design = NSFG.svydes)
#' cv.svyglm(glm_object = NSFG.svyglm, nfolds = 4)
#' @export

# TODO: Write formal unit tests

cv.svyglm <- function(glm_object, nfolds = 5) {

  family <- glm_object$family$family
  stopifnot(family %in% c("gaussian", "quasibinomial"))
  method <- if (family == "gaussian") {
    "linear"
  } else if (family == "quasibinomial" ) {
      "logistic"
  }

  formulae <- deparse1(glm_object[["formula"]])
  design_object <- glm_object[["survey.design"]]

  # Runs our cv.svydesign() function using the pieces pulled from the glm object,
  # which will later push all of this information into our general cv.svy() function.
  cv.svydesign(design_object = design_object, formulae = formulae,
               nfolds = nfolds, method = method)

}

