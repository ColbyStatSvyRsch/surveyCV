#' Stratified function
#'
#' this is a cross validation function designed for survey samples taken using a
#' stratified sampling design
#'
#' @param Data CSV of dataset to be tested
#' @param formulae Vector of formulas containing the variables to be compared in
#'   cross validation
#' @param nfolds Number of folds to be used during cross validation, defaults to
#'   5
#' @param strataID String of the variable used to Stratify during sampling, must
#'   be the same as in the dataset used
#' @param N Number equal to the total population size
#' @param method string, must be either linear or logistic, determines type of
#'   model fit during cross validation, defaults to linear
#'  @examples
#' #MSEs generated for a stratified test of a first and second degree polynomial
#' # fit predicting mpg from horsepower in the Auto Dataset, Stratified on the
#' # "year" variable
#' data("Auto")
#' cv.strat.lm(Auto, c("mpg~poly(horsepower,1, raw = TRUE)", "mpg~poly(horsepower,2, raw = TRUE)"), nfolds = 10, "year", 400)
#' @export


# Cross Val for Stratafication
cv.strat.lm <- function(Data, formulae, nfolds=5, strataID, N, method = "linear") {
  # Other option for method is "logistic"

  #stop logic checking dataset specified
  if(nfolds < 1) {print ("nfolds is less that 1")}
  if(N<nrow(Data)) {print("N is less than observations in the defined dataset")}
  if(nfolds >= nrow(Data)) {print ("fold number exceeds observations")}

  stopifnot(nfolds > 0, N >= nrow(Data), nfolds <= nrow(Data))

  # Turns the strings of formulas into a list of formulas
  formulae <- sapply(formulae, as.formula)
  # Pushes the data into this full_ds data frame
  full_ds <- Data
  # Creates an observation ID variable for the dataset
  full_ds$ID <- 1:nrow(full_ds)
  # Creates a new dataset that is a copy of full_ds that we can now scramble
  # and then order by the variable we are stratifying on
  randomized_ds <- data.frame(ID = full_ds$ID, stratumID = full_ds[[strataID]])
  randomized_ds <- randomized_ds[sample(1:nrow(randomized_ds)),]
  randomized_ds <- randomized_ds[order(randomized_ds$stratumID),]
  # Assigns a fold ID to to all of the observations
  randomized_ds$foldID <- rep(1:nfolds, length.out=nrow(randomized_ds))
  # Orders the scrambled datset by the observation ID so that this dataset is in orginal order,
  # and then attches the fold ID variable to the full_ds data frame
  randomized_ds <- randomized_ds[order(randomized_ds$ID),]
  full_ds$foldID <- randomized_ds$foldID
  # Makes a matrix that the test errors squared will be pumped back into inside the for loop
  test_errors_sq <- matrix(NA, nrow=nrow(full_ds), ncol=length(formulae))
  # This loops through each fold to create a training dataset and holdout (test) dataset for that
  # k-fold, while also making the svydesign for that fold based on the training dataset
  for (fold in 1:nfolds) {
    test.rows <- which(full_ds$foldID == fold)
    train <- full_ds[-test.rows,]
    test <- full_ds[test.rows,]
    n = nrow(train)
    strat.svy <- svydesign(ids = ~0,
                           strata = formula(paste0('~',strataID)),
                           fpc = rep(N, n),
                           data = train)
    # This loops through the formulas in our list of formulas and calculates the test errors
    # squared for thos formulas applied to each survey design made from each fold and plugs
    # those test errors squared back into the matrix we made earlier
    if (method == "linear") {
      for (form in 1:length(formulae)) {
        current.model <- svyglm(formula=formulae[[form]], design=strat.svy)
        predictions <- predict(current.model, newdata=test)
        test.responses <- eval(formulae[[form]][[2]], envir=test)
        test.errors <- test.responses - predictions
        test_errors_sq[test$ID, form] <- test.errors^2
      }
    } else if (method == "logistic") {
      for (form in 1:length(formulae)) {
        current.model <- svyglm(formula=formulae[[form]], design=strat.svy, family = quasibinomial())
        predictions <- predict(current.model, newdata=test, type="response")
        test.responses <- eval(formulae[[form]][[2]], envir=test)
        test_errors_sq[test$ID, form] <- -(test.responses * log(predictions) + (1-test.responses) * log(1-predictions))
      }
    }
  }
  # This converts our matrix into a data frame so it can more easily manipulated
  test_errors_sq.df <- as.data.frame(test_errors_sq)
  # Attaches our test errors squared back onto the original dataset
  complete_data <- cbind(full_ds, test_errors_sq.df)
  # Makes a survey design for based off of the whole dataset so we can calculate a mean
  strat.svy <- svydesign(ids = ~0,
                         strata = formula(paste0('~',strataID)),
                         fpc = rep(N, nrow(complete_data)),
                         data = complete_data)
  # Makes an empty matrix that we can pump the means and SE into for each formula by row
  means <- matrix(NA, nrow=(ncol(complete_data) - ncol(full_ds)), ncol=2)
  # Sets i for the loop to start at the first column in the dataset that contains test errors sq
  i = ncol(full_ds) + 1
  # Sets the loop to start on column one of the matrix
  y = 1
  # Makes a data frame of the output from the svymean function and then plugs the Mean output
  # into the first column of the matrix and the SE output into the second column of the matrix
  while (i <= ncol(complete_data)) {
    meansd <- data.frame(svymean(complete_data[,i],strat.svy))
    means[y,1] <- meansd$mean
    means[y,2] <- meansd$SE
    i = i+1
    y = y+1
  }
  # Returns the resulting matrix to the console
  return(means)
}

