#' CV for survey data
#'
#' This is a cross validation function designed for survey samples taken using a SRS,
#' stratified, clustered, or clustered-and-stratified sampling design.
#' Returns survey CV estimates of the mean loss for each model
#' (MSE for linear models, or binary cross-entropy for logistic models).
#'
#' If you have already created a \code{svydesign} object or fitted a \code{svyglm},
#' you will probably prefer the convenience wrapper functions
#' \code{\link{cv.svydesign}} or \code{\link{cv.svyglm}}.
#'
#' For models other than linear or logistic regression,
#' you can use \code{\link{folds.svy}} or \code{\link{folds.svydesign}} to generate
#' CV fold IDs that respect any stratification or clustering in the survey design.
#' You can then carry out K-fold CV as usual,
#' taking care to also use the survey design features and survey weights
#' when fitting models in each training set
#' and also when evaluating models against each test set.
#'
#' @param Data Dataframe of dataset to be used for CV
#' @param formulae Vector of formulas (as strings) for the GLMs to be compared in
#'   cross validation
#' @param nfolds Number of folds to be used during cross validation, defaults to
#'   5
#' @param strataID String of the variable name used to stratify during sampling, must
#'   be the same as in the dataset used
#' @param clusterID String of the variable name used to cluster during sampling, must
#'   be the same as in the dataset used
#' @param nest Specify nest = TRUE if clusters are nested within strata, defaults to FALSE
#' @param fpcID String of the variable name used for finite population corrections, must
#'   be the same as in the dataset used, see \code{\link[survey]{svydesign}} for details
#' @param method String, must be either "linear" or "logistic", determines type of
#'   model fit during cross validation, defaults to linear
#' @param weightsID String of the variable name in the dataset that contains sampling weights
#' @param useSvyForFolds Specify useSvyForFolds = TRUE (default) to take svydesign into account when making folds;
#'   should not be set FALSE except for running simulations to understand the properties of surveyCV
#' @param useSvyForFits Specify useSvyForFits = TRUE (default) to take svydesign into account when fitting models on training sets;
#'   should not be set FALSE except for running simulations to understand the properties of surveyCV
#' @param useSvyForLoss Specify useSvyForLoss = TRUE (default) to take svydesign into account when calculating loss over test sets;
#'   should not be set FALSE except for running simulations to understand the properties of surveyCV
#' @param na.rm Whether to drop cases with missing values when taking `svymean`
#'   of test losses
#' @return Object of class \code{svystat}, which is a named vector of survey CV estimates of the mean loss
#'   (MSE for linear models, or binary cross-entropy for logistic models) for each model,
#'   with names ".Model_1", ".Model_2", etc. corresponding to the models provided in \code{formulae};
#'   and with a \code{var} attribute giving the variances.
#'   See \code{\link[survey]{surveysummary}} for details.
#' @seealso \code{\link[survey]{surveysummary}}, \code{\link[survey]{svydesign}}
#' @seealso \code{\link{cv.svydesign}} for a wrapper to use with a \code{svydesign} object,
#'   or \code{\link{cv.svyglm}} for a wrapper to use with a \code{svyglm} object
#' @examples
#' # Compare CV MSEs and their SEs under 3 linear models
#' # for a stratified sample and a one-stage cluster sample,
#' # using data from the `survey` package
#' library(survey)
#' data("api", package = "survey")
#' # stratified sample
#' cv.svy(apistrat, c("api00~ell",
#'                    "api00~ell+meals",
#'                    "api00~ell+meals+mobility"),
#'        nfolds = 5, strataID = "stype", weightsID = "pw", fpcID = "fpc")
#' # one-stage cluster sample
#' cv.svy(apiclus1, c("api00~ell",
#'                    "api00~ell+meals",
#'                    "api00~ell+meals+mobility"),
#'        nfolds = 5, clusterID = "dnum", weightsID = "pw", fpcID = "fpc")
#'
#' # Compare CV MSEs and their SEs under 3 linear models
#' # for a stratified cluster sample with clusters nested within strata
#' data(NSFG_data)
#' library(splines)
#' cv.svy(NSFG_data, c("income ~ ns(age, df = 2)",
#'                     "income ~ ns(age, df = 3)",
#'                     "income ~ ns(age, df = 4)"),
#'        nfolds = 4,
#'        strataID = "strata", clusterID = "SECU",
#'        nest = TRUE, weightsID = "wgt")
#'
#' # Logistic regression example, using the same stratified cluster sample;
#' # instead of CV MSE, we calculate CV binary cross-entropy loss,
#' # where (as with MSE) lower values indicate better fitting models
#' # (NOTE: na.rm=TRUE is not usually ideal;
#' #  it's used below purely for convenience, to keep the example short,
#' #  but a thorough analysis would look for better ways to handle the missing data)
#' cv.svy(NSFG_data, c("KnowPreg ~ ns(age, df = 1)",
#'                     "KnowPreg ~ ns(age, df = 2)",
#'                     "KnowPreg ~ ns(age, df = 3)"),
#'        method = "logistic", nfolds = 4,
#'        strataID = "strata", clusterID = "SECU",
#'        nest = TRUE, weightsID = "wgt",
#'        na.rm = TRUE)
#' @export


# TODO: Improve the API / interface: be consistent with `survey`, with tidyverse,
#       and with other packages that do cross-validation (or at least SOME of these)
#       See for instance https://github.com/gergness/srvyr/issues/140
#       about `srvyr` and `tidymodels`

# TODO: Allow other svydesign features besides clusters, strata, weights, and nest,
#       such as pps options, or the option to specify probs instead of weights

# TODO: Write formal unit tests

# TODO: Add other GLMs (eg Poisson) and other models built in to `survey`;
#       or is there a better way to write our code so it "just works" no matter what model,
#       so users don't have to specify linear, logistic, etc.?

# TODO: Add more error-handling checks on response variables:
#       for each formula in formulae, are the response vars the same?
#         (if not, we shouldn't be comparing them with CV)
#       and are they either all continuous (for linear model)
#         or all 0/1 or binary factor (for logistic)?
#       and for logistic, are there enough of each class to fit models?
#         (glmnet complains if any class has 0 or 1 observations;
#          but we might also need to do that *within each fold*,
#          so possibly might need to rerun appendfolds() a few times
#          or build in an option to stratify on response???)

# TODO: Condense the ways we create train.svydes and full.svydes to avoid code copies?
#       Can we replace the hardcoded train.svydesign details
#       with just subset.survey.design() ???

# TODO: Condense the ways we calculate test errors / losses for linear vs logistic, to avoid code copies?




# General Cross Validation
cv.svy <- function(Data, formulae, nfolds=5, strataID = NULL, clusterID = NULL, nest = FALSE, fpcID = NULL,
                   method = c("linear", "logistic"), weightsID = NULL,
                   useSvyForFolds = TRUE, useSvyForFits = TRUE, useSvyForLoss = TRUE,
                   na.rm = FALSE) {

  method <- match.arg(method)
  stopifnot(nfolds >= 1, nfolds <= nrow(Data))

  # Turns the strings of formulas into a list of formulas
  formulae <- sapply(formulae, as.formula)
  nformulae <- length(formulae)
  # Creates an observation ID variable for the dataset
  Data$.ID <- 1:nrow(Data)
  # Runs our fold generation function
  if (useSvyForFolds == TRUE) {
    Data <- cbind(Data, .foldID = folds.svy(Data = Data, nfolds = nfolds, strataID = strataID, clusterID = clusterID))
  } else  {
    Data <- cbind(Data, .foldID = folds.svy(Data = Data, nfolds = nfolds))
  }

  # Makes a matrix that the test losses will be pumped back into inside the for loop
  .test_loss <- matrix(NA, nrow=nrow(Data), ncol=nformulae)
  colnames(.test_loss) <- paste0(".Model_", 1:nformulae)
  # This loops through each fold to create a training dataset and holdout (test) dataset for that
  # k-fold, while also making the svydesign for that fold based on the training dataset
  for (fold in 1:nfolds) {
    test.rows <- which(Data$.foldID == fold)
    train <- Data[-test.rows,]
    test <- Data[test.rows,]
    n <- nrow(train)
    train.svydes <- svydesign(
      ids = if(is.null(clusterID) | !useSvyForFits) formula(~0) else formula(paste0("~", clusterID)),
      strata = if(is.null(strataID) | !useSvyForFits) NULL else formula(paste0("~", strataID)),
      fpc = if(is.null(fpcID) | !useSvyForFits) NULL else formula(paste0("~", fpcID)),
      weights = if(is.null(weightsID) | !useSvyForFits) NULL else formula(paste0("~", weightsID)),
      nest = nest,
      data = train)
    # This loops through the formulas in our list of formulas and calculates the test losses
    # for those formulas applied to each survey design made from each fold and plugs
    # those test losses back into the matrix we made earlier
    if (method == "linear") {
      for (form in 1:nformulae) {
        current.model <- svyglm(formula=formulae[[form]], design=train.svydes)
        predictions <- predict(current.model, newdata=test)
        test.responses <- eval(formulae[[form]][[2]], envir=test)
        test.errors <- test.responses - predictions
        .test_loss[test$.ID, form] <- test.errors^2
      }
    } else if (method == "logistic") {
      for (form in 1:nformulae) {
        current.model <- svyglm(formula=formulae[[form]], design=train.svydes, family = quasibinomial())
        predictions <- predict(current.model, newdata=test, type="response")
        test.responses <- eval(formulae[[form]][[2]], envir=test)
        if(is.factor(test.responses)) {
          stopifnot(length(levels(test.responses)) == 2)
          # If y is a factor, R's glm and svyglm internally use a dummy var with
          # values of 0 for the baseline level and 1 for the non-baseline;
          # we must do the same to eval test.responses against numeric predictions
          test.responses <- as.numeric(test.responses == levels(test.responses)[2])
        }
        .test_loss[test$.ID, form] <- -(test.responses * log(predictions) + (1-test.responses) * log(1-predictions))
      }
    }
  }

  # This converts our matrix into a data frame so it can more easily manipulated
  test_loss.df <- as.data.frame(.test_loss)
  # Attaches our test losses back onto the original dataset
  Data <- cbind(Data, test_loss.df)
  # Makes a survey design based off of the whole dataset so we can calculate a mean
  full.svydes <- svydesign(
    ids = if(is.null(clusterID) | !useSvyForLoss) formula(~0) else formula(paste0("~", clusterID)),
    strata = if(is.null(strataID) | !useSvyForLoss) NULL else formula(paste0("~", strataID)),
    fpc = if(is.null(fpcID) | !useSvyForLoss) NULL else formula(paste0("~", fpcID)),
    weights = if(is.null(weightsID) | !useSvyForLoss) NULL else formula(paste0("~", weightsID)),
    nest = nest,
    data = Data)

  # Get the names of the last nformulae columns of Data:
  # each of these columns is the .test_loss values for one of the formulae
  whichvars <- names(Data)[ncol(Data) - nformulae + (1:nformulae)]
  # Use make.formula() to tell svymean() to estimate their means and SEs
  CVmeans <- svymean(make.formula(whichvars), full.svydes,
                     na.rm = na.rm)
  # Return the resulting svystat object
  return(CVmeans)
}

