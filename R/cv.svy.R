#' CV for survey data
#'
#' This is a cross validation function designed for survey samples taken using a SRS,
#' stratified, clustered, or clustered-and-stratified sampling design.
#'
#' @param Data Dataframe of dataset to be tested
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
#'   be the same as in the dataset used, see `?svydesign` for details
#' @param method String, must be either "linear" or "logistic", determines type of
#'   model fit during cross validation, defaults to linear
#' @param weightsID String of the variable name in the dataset that contains sampling weights
#' @param useSvyForFolds Specify useSvyForFolds = TRUE (default) to take svydesign into account when making folds;
#'   should not be set FALSE except for running simulations to understand the properties of surveyCV
#' @param useSvyForFits Specify useSvyForFits = TRUE (default) to take svydesign into account when fitting models on training sets;
#'   should not be set FALSE except for running simulations to understand the properties of surveyCV
#' @param useSvyForLoss Specify useSvyForLoss = TRUE (default) to take svydesign into account when calculating loss over test sets;
#'   should not be set FALSE except for running simulations to understand the properties of surveyCV
#' @examples
#' # MSEs generated for a stratified test of a first and second degree polynomial
#' # fit predicting mpg from horsepower in the Auto dataset, stratified on the
#' # "year" variable
#' data("Auto", package = "ISLR")
#' cv.svy(Auto, c("mpg~poly(horsepower,1, raw = TRUE)",
#'                "mpg~poly(horsepower,2, raw = TRUE)"),
#'        nfolds = 10, strataID = "year")
#' @export


# TODO: Improve the API / interface: be consistent with `survey`, with tidyverse,
#       and with other packages that do cross-validation (or at least SOME of these)

# TODO: Allow other svydesign features besides clusters, strata, weights, and nest,
#       such as pps options, or the option to specify probs instead of weights

# TODO: Write formal unit tests

# TODO: Add other GLMs (eg Poisson) and other models built in to `survey`;
#       or is there a better way to write our code so it "just works" no matter what model,
#       so users don't have to specify linear, logistic, etc.?

# TODO: Condense the ways we create train.svydes and full.svydes to avoid code copies?

# TODO: Condense the ways we calculate test errors / losses for linear vs logistic, to avoid code copies?




# General Cross Validation
cv.svy <- function(Data, formulae, nfolds=5, strataID = NULL, clusterID = NULL, nest = FALSE, fpcID = NULL,
                   method = c("linear", "logistic"), weightsID = NULL,
                   useSvyForFolds = TRUE, useSvyForFits = TRUE, useSvyForLoss = TRUE) {

  # Use stop-logic to check the dataset & arguments specified
  if(nfolds < 1) {print ("nfolds is less that 1")}
  if(nfolds >= nrow(Data)) {print ("Number of folds exceeds observations")}
  method <- match.arg(method)

  stopifnot(nfolds > 0, nfolds <= nrow(Data))

  # Turns the strings of formulas into a list of formulas
  formulae <- sapply(formulae, as.formula)
  nformulae <- length(formulae)
  # Creates an observation ID variable for the dataset
  Data$.ID <- 1:nrow(Data)
  # Runs our fold generation function seen in utils
  if (useSvyForFolds == TRUE) {
    Data <- appendfolds(Data = Data, nfolds = nfolds, strataID = strataID, clusterID = clusterID)
  } else  {
    Data <- appendfolds(Data = Data, nfolds = nfolds)
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
  CVmeans <- svymean(make.formula(whichvars), full.svydes)
  # Return the resulting svystat object
  return(CVmeans)
}

