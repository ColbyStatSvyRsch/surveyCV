#' SRS Function
#'
#' This is a cross validation function for survey samples taken using a a simple
#' random sample survey design
#'
#' @param Data CSV of dataset to be tested
#' @param formulae Vector of formulas containing the variables to be compared in
#'   cross validation
#' @param nfolds Number of folds to be used during cross validation, defaults to
#'   5
#' @param N Number equal to the total population size
#' @param method string, must be either linear or logistic, determines type of
#'   model fit during cross validation, defaults to linear
#' @param weights Variable in data set that contains PPS weights
#' @examples
#' #MSEs generated for a SRS test of a first and second degree polynomial
#' # fit predicting mpg from horsepower in the Auto Dataset
#' data("Auto")
#' cv.srs.lm(Auto, c("mpg~poly(horsepower,1, raw = TRUE)", "mpg~poly(horsepower,2, raw = TRUE)"), nfolds = 10, N = 400)
#' @export


# Cross Val for SRS
cv.srs.lm <- function(Data, formulae, nfolds=5, N, method = "linear", weights =  NULL) {
  # Other option for method is "logistic"

  #stop logic checking dataset specified
  if(nfolds < 1) {print ("nfolds is less that 1")}
  if(N<nrow(Data)) {print("N is less than observations in the defined dataset")}
  if(nfolds > nrow(Data)) {print ("fold number exceeds observations")}
  stopifnot(nfolds > 0, N >= nrow(Data), nfolds < nrow(Data))


  # Creates an observation ID variable for the dataset
  Data$ID <- 1:nrow(Data)
  # Turns the strings of formulas into a list of formulas
  formulae <- sapply(formulae, as.formula)
  # Assigns the sample size to n
  n <- nrow(Data)
  # Creates a fold.labels list that will contain folds 1 through nfolds
  fold.labels <- sample(rep(1:nfolds, length.out=n))
  # Makes a matrix that the test errors squared will be pumped back into inside the for loop
  test_errors_sq <- matrix(NA, nrow=nrow(Data), ncol=length(formulae))
  # This loops through each fold to create a training dataset and holdout (test) dataset for that
  # k-fold, while also making the svydesign for that fold based on the training dataset
  for (fold in 1:nfolds) {
    test.rows <- which(fold.labels == fold)
    train <- Data[-test.rows,]
    test <- Data[test.rows,]
    if (is.null(weights) == TRUE) {
      srs.svy <- svydesign(ids = ~0,
                           strata = NULL,
                           fpc = rep(N, nrow(train)),
                           data = train)
      } else {
      srs.svy <- svydesign(ids = ~0,
                           strata = NULL,
                           fpc = rep(N, nrow(train)),
                           weights = weights,
                           data = train)
      }
    # This loops through the formulas in our list of formulas and calculates the test errors
    # squared for thos formulas applied to each survey design made from each fold and plugs
    # those test errors squared back into the matrix we made earlier
    if (method == "linear") {
      for (form in 1:length(formulae)) {
        current.model <- svyglm(formula=formulae[[form]], design=srs.svy)
        predictions <- predict(current.model, newdata=test)
        test.responses <- eval(formulae[[form]][[2]], envir=test)
        test.errors <- test.responses - predictions
        test_errors_sq[test$ID, form] <- test.errors^2
      }
    } else if (method == "logistic") {
      for (form in 1:length(formulae)) {
        current.model <- svyglm(formula=formulae[[form]], design=srs.svy, family = quasibinomial())
        predictions <- predict(current.model, newdata=test, type="response")
        test.responses <- eval(formulae[[form]][[2]], envir=test)
        test_errors_sq[test$ID, form] <- -(test.responses * log(predictions) + (1-test.responses) * log(1-predictions))
      }
    }
  }
  # This converts our matrix into a data frame so it can more easily manipulated
  test_errors_sq.df <- as.data.frame(test_errors_sq)
  # Attaches our test errors squared back onto the original dataset
  complete_data <- cbind(Data, test_errors_sq.df)
  # Makes a survey design for based off of the whole dataset so we can calculate a mean
  if (is.null(weights) == TRUE) {
    srs.svy <- svydesign(ids = ~0,
                         strata = NULL,
                         fpc = rep(N, nrow(complete_data)),
                         data = complete_data)
  } else {
    srs.svy <- svydesign(ids = ~0,
                         strata = NULL,
                         fpc = rep(N, nrow(complete_data)),
                         weights = weights,
                         data = complete_data)}
  # Makes an empty matrix that we can pump the means and SE into for each formula by row
  means <- matrix(NA, nrow=(ncol(complete_data) - ncol(Data)), ncol=2)
  # Sets i for the loop to start at the first column in the dataset that contains test errors sq
  i = ncol(Data) + 1
  # Sets the loop to start on column one of the matrix
  y = 1
  # Makes a data frame of the output from the svymean function and then plugs the Mean output
  # into the first column of the matrix and the SE output into the second column of the matrix
  while (i <= ncol(complete_data)) {
    meansd <- data.frame(svymean(complete_data[,i],srs.svy))
    means[y,1] <- meansd$mean
    means[y,2] <- meansd$SE
    i = i+1
    y = y+1
  }
  # Returns the resulting matrix to the console
  return(means)
}

