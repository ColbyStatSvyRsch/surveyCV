#' General CV function
#'
#' This is a cross validation function designed for survey samples taken using a SRS,
#' stratified, clustered, or clustered-and-stratified sampling design.
#'
#' @param Data Dataframe of dataset to be tested
#' @param formulae Vector of formulas for the GLMs to be compared in
#'   cross validation
#' @param nfolds Number of folds to be used during cross validation, defaults to
#'   5
#' @param strataID String of the variable name used to stratify during sampling, must
#'   be the same as in the dataset used
#' @param clusterID String of the variable name used to cluster during sampling, must
#'   be the same as in the dataset used
#' @param N Total finite population size
#' @param method String, must be either "linear" or "logistic", determines type of
#'   model fit during cross validation, defaults to linear
#' @param weights String of the variable name in the dataset that contains sampling weights
#' @param nest Specify nest = TRUE if clusters are nested within strata, defaults to FALSE
#' @param useSvyForFolds Specify useSvyForFolds = TRUE (default) to take svydesign into account when making folds;
#'   should not be set FALSE except for running simulations to understand the properties of surveyCV
#' @examples
#' # MSEs generated for a stratified test of a first and second degree polynomial
#' # fit predicting mpg from horsepower in the Auto dataset, stratified on the
#' # "year" variable
#' data("Auto", package = "ISLR")
#' cv.svy(Auto, c("mpg~poly(horsepower,1, raw = TRUE)",
#'                "mpg~poly(horsepower,2, raw = TRUE)"),
#'        nfolds = 10, strataID = "year", N = 400)
#' @export


# General Cross Validation
cv.svy <- function(Data, formulae, nfolds=5, strataID = NULL, clusterID = NULL , nest = FALSE, N,
                   method = c("linear", "logistic"), weights = NULL, useSvyForFolds = TRUE) {

  # Use stop-logic to check the dataset & arguments specified
  if(nfolds < 1) {print ("nfolds is less that 1")}
  if(N<nrow(Data)) {print("N is less than observations in the defined dataset")}
  if(nfolds >= nrow(Data)) {print ("Number of folds exceeds observations")}
  method <- match.arg(method)

  stopifnot(nfolds > 0, N >= nrow(Data), nfolds <= nrow(Data))

  # Turns the strings of formulas into a list of formulas
  formulae <- sapply(formulae, as.formula)
  # Creates an observation ID variable for the dataset
  Data$.ID <- 1:nrow(Data)
  # Runs our fold generation function seen in utils
  if (useSvyForFolds == TRUE) {
    Data <- appendfolds(Data = Data, nfolds = nfolds, strataID = strataID, clusterID = clusterID)
  } else  {
    Data <- appendfolds(Data = Data, nfolds = nfolds)
  }

  # Makes a matrix that the test losses will be pumped back into inside the for loop
  .test_loss <- matrix(NA, nrow=nrow(Data), ncol=length(formulae))
  # This loops through each fold to create a training dataset and holdout (test) dataset for that
  # k-fold, while also making the svydesign for that fold based on the training dataset
  for (fold in 1:nfolds) {
    test.rows <- which(Data$.foldID == fold)
    train <- Data[-test.rows,]
    test <- Data[test.rows,]
    n <- nrow(train)
    train.svydes <- svydesign(ids = if(is.null(clusterID)) formula(~0) else formula(paste0("~", clusterID)),
                              strata = if(is.null(strataID)) NULL else formula(paste0("~", strataID)),
                              fpc = rep(N, nrow(train)),
                              weights = if(is.null(weights)) NULL else formula(paste0("~", weights)),
                              nest = nest,
                              data = train)
    # This loops through the formulas in our list of formulas and calculates the test losses
    # for those formulas applied to each survey design made from each fold and plugs
    # those test losses back into the matrix we made earlier
    if (method == "linear") {
      for (form in 1:length(formulae)) {
        current.model <- svyglm(formula=formulae[[form]], design=train.svydes)
        predictions <- predict(current.model, newdata=test)
        test.responses <- eval(formulae[[form]][[2]], envir=test)
        test.errors <- test.responses - predictions
        .test_loss[test$.ID, form] <- test.errors^2
      }
    } else if (method == "logistic") {
      for (form in 1:length(formulae)) {
        current.model <- svyglm(formula=formulae[[form]], design=train.svydes, family = quasibinomial())
        predictions <- predict(current.model, newdata=test, type="response")
        test.responses <- eval(formulae[[form]][[2]], envir=test)
        .test_loss[test$.ID, form] <- -(test.responses * log(predictions) + (1-test.responses) * log(1-predictions))
      }
    }
  }
  # This converts our matrix into a data frame so it can more easily manipulated
  test_loss.df <- as.data.frame(.test_loss)
  # Attaches our test losses back onto the original dataset:
  # append right into data$test_loss
  Data <- cbind(Data, test_loss.df)
  # Makes a survey design for based off of the whole dataset so we can calculate a mean
  full.svydes <- svydesign(ids = if(is.null(clusterID)) formula(~0) else formula(paste0("~", clusterID)),
                           strata = if(is.null(strataID)) NULL else formula(paste0("~", strataID)),
                           fpc = rep(N, nrow(Data)),
                           weights = if(is.null(weights)) NULL else formula(paste0("~", weights)),
                           nest = nest,
                           data = Data)

  # Makes an empty matrix that we can pump the means and SE into for each formula by row
  means <- matrix(NA, nrow=length(formulae), ncol=2)
  # Sets i for the loop to start at the first column in the dataset that contains test losses
  i = ncol(Data) - length(formulae) + 1
  # Sets the loop to start on column one of the matrix
  y = 1
  # Makes a data frame of the output from the svymean function and then plugs the Mean output
  # into the first column of the matrix and the SE output into the second column of the matrix
  while (i <= ncol(Data)) {
    meansd <- data.frame(svymean(Data[,i], full.svydes))
    means[y,1] <- meansd$mean
    means[y,2] <- meansd$SE
    i = i+1
    y = y+1
  }
  # Returns the resulting matrix to the console
  return(means)
}

