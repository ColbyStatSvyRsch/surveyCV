#' Cluster Function
#'
#' This is a cross validation function designed for samples taken using a
#' clustered sampling design
#' @export


# Cross Val for Clusters
cv.cluster.lm <- function(Data, formulae, nfolds=5, clusterID, N, method = "linear") {
  # Other option for method is "logistic"

  #stop logic checking dataset specified
  if(nfolds < 1) {print ("nfolds is less that 1")}
  if(N<nrow(Data)) {print("N is less than observations in the defined dataset")}
  if(nfolds >= nrow(Data)) {print ("fold number exceeds observations")}
  if(nfolds > length(unique(Data[[clusterID]]))) {print ("nfolds is larger than the number of clusters")}
  stopifnot(nfolds > 0, N >= nrow(Data), nfolds <= nrow(Data), nfolds <= length(unique(Data[[clusterID]])))

  # Turns the strings of formulas into a list of formulas
  formulae <- sapply(formulae, as.formula)
  # Assigns the sample size to n
  n <- nrow(Data)
  # Creates an observation ID variable for the dataset
  Data$ID <- 1:nrow(Data)
  # Takes all of the cluster IDs and randomizes their order
  unique_cluster_IDs  <- sample(unique(Data[[clusterID]]))
  # The split() function cuts up the unique_cluster_IDs into an nfold number lists that contain
  # the cluster IDs. So Clusters becomes a list of these lists, or a list of the folds.
  Clusters <- split(unique_cluster_IDs, factor(sort(rank(unique_cluster_IDs)%%nfolds)))
  # Sets x=1 for the start of the loop so it will start on fold 1
  x=1
  # Assigns a fold ID to the original dataset just so that the variable alrady exhists before
  # the while loop. This information will be replaced whenthe while loop runs.
  Data$foldID <- 1:nrow(Data)
  # This loop runs through each of the number of folds to try and place whole clusters into
  # each fold
  while (x < (nfolds+1)) {
    sample_cluster <- Clusters[[x]]
    i = 1
    # This loop runs through each cluster and assigns a fold ID to an entire cluster at a time
    while (i <= length(sample_cluster)) {
      Data$foldID[Data[[clusterID]] == sample_cluster[[i]]] <- x
      i = i+1
    }
    x = x+1
  }
  # Makes a matrix that the test errors squared will be pumped back into inside the for loop
  test_errors_sq <- matrix(NA, nrow=nrow(Data), ncol=length(formulae))
  # This loops through each fold to create a training dataset and holdout (test) dataset for that
  # k-fold, while also making the svydesign for that fold based on the training dataset
  for (fold in 1:nfolds) {
    test.rows <- which(Data$foldID == fold)
    train <- Data[-test.rows,]
    test <- Data[test.rows,]
    n <- nrow(train)
    clus.svy <- svydesign(ids = formula(paste0("~",clusterID)),
                          strata = NULL,
                          fpc = rep(N, n),
                          data = train)
    # This loops through the formulas in our list of formulas and calculates the test errors
    # squared for thos formulas applied to each survey design made from each fold and plugs
    # those test errors squared back into the matrix we made earlier
    if (method == "linear") {
      for (form in 1:length(formulae)) {
        current.model <- svyglm(formula=formulae[[form]], design=clus.svy)
        predictions <- predict(current.model, newdata=test)
        test.responses <- eval(formulae[[form]][[2]], envir=test)
        test.errors <- test.responses - predictions
        test_errors_sq[test$ID, form] <- test.errors^2
      }
    } else if (method == "logistic") {
      for (form in 1:length(formulae)) {
        current.model <- svyglm(formula=formulae[[form]], design=clus.svy, family = quasibinomial())
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
  clus.svy <- svydesign(ids = formula(paste0("~",clusterID)),
                        strata = NULL, ## Figure out weights later
                        fpc = rep(N, nrow(complete_data)),
                        data = complete_data)
  # Makes an empty matrix that we can pump the means and SE into for each formula by row
  means <- matrix(NA, nrow=(ncol(complete_data) - ncol(Data)), ncol=2)
  # Sets i for the loop to start at the first column in the dataset that contains test errors sq
  i = ncol(Data) + 1
  # Sets the loop to start on column one of the matrix
  y = 1
  # Makes a data frame of the output from the svymean function and then plugs the Mean output
  # into the first column of the matrix and the SE output into the second column of the matrix
  while (i <= ncol(complete_data)) {
    meansd <- data.frame(svymean(complete_data[,i],clus.svy))
    means[y,1] <- meansd$mean
    means[y,2] <- meansd$SE
    i = i+1
    y = y+1
  }
  # Returns the resulting matrix to the console
  return(means)
}

