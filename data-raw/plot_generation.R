# CAUTION: the functions below will set.seed() several times,
# so don't try to do anything random after running them --
# or set.seed() randomly yourself right afterwards.

# Packages needed to run all functions below:
#   dplyr, ggplot2, gridExtra, sampling, survey
# (We originally did not load the packages directly up here
#  because we had wanted this to be part of our surveyCV package,
#  showing up in a vignette,
#  and were trying to work around namespace issues...
#  but now that these have become "old" prelim results,
#  we've moved them out of the way to data-raw folder
#  so they should be ignored during package building.)


plot_generation <- function() {

  set.seed(47)
  x1 = stats::runif(1:500, min = 26, max = 38)
  y1 = (x1-29)^3 - 13*(x1-29)^2 + 0*(x1-29) + 900

  set.seed(47)
  x2 = stats::runif(1:500, min = 38, max = 50)
  y2 = (x2-36)^3 - 10*(x2-36)^2 + 2*(x2-36) + 600

  set.seed(47)
  z1 = jitter(y1, 15000)
  z1 = jitter(y1, 15000)
  z2 = jitter(y2, 15000)

  ds1 <- data.frame(Response = z1, Predictor = x1)
  ds2 <- data.frame(Response = z2, Predictor = x2)

  ds <- rbind(ds1, ds2)

  b <- data.frame(ID = c(1:1000))
  spline.df2 <- cbind(b, ds)
  spline.df2 <- spline.df2 %>%
    dplyr::arrange(Predictor) %>%
    dplyr::mutate(Stratum = dplyr::row_number(),
                  Cluster = dplyr::row_number())
  spline.df2$Stratum <- cut(spline.df2$Stratum,5, 1:5)
  spline.df2$Cluster <- cut(spline.df2$Cluster,100, 1:100)
  spline.df2 <- spline.df2 %>%
    dplyr::arrange(ID) %>%
    dplyr::select(ID, Response, Predictor, Cluster, Stratum)

  # Cross Val for SRS and Strat/Cluster
  cv.srs.mixed.lm <- function(Data, formulae, nfolds=5, N, scID, method = "linear", model = "SRS", error_calc = "SRS"){
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
    # Creates a fold.labels list that will contain folds 1 through nfolds
    fold.labels <- sample(rep(1:nfolds, length.out=nrow(Data)))
    # Makes a matrix that the test errors squared will be pumped back into inside the for loop
    test_errors_sq <- matrix(NA, nrow=nrow(Data), ncol=length(formulae))
    # This loops through each fold to create a training dataset and holdout (test) dataset for that
    # k-fold, while also making the svydesign for that fold based on the training dataset
    for (fold in 1:nfolds) {
      test.rows <- which(fold.labels == fold)
      train <- Data[-test.rows,]
      test <- Data[test.rows,]
      srs.svy <- svydesign(ids = ~0,
                           strata = NULL,
                           fpc = rep(N, nrow(train)),
                           data = train)

      ###
      strat.svy  <- svydesign(ids = ~0,
                              strata = formula(paste0('~',scID)),
                              fpc = rep(N, nrow(train)),
                              data = train)
      ###
      clus.svy <- svydesign(ids = formula(paste0("~",scID)),
                            strata = NULL,
                            fpc = rep(N, nrow(train)),
                            data = train)
      # This loops through the formulas in our list of formulas and calculates the test errors
      # squared for thos formulas applied to each survey design made from each fold and plugs
      # those test errors squared back into the matrix we made earlier
      if (model == "SRS") {
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
      }else if (model == "Strat"){
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
      }else if (model == "Cluster"){
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
    }
    # This converts our matrix into a data frame so it can more easily manipulated
    test_errors_sq.df <- as.data.frame(test_errors_sq)
    # Attaches our test errors squared back onto the original dataset
    complete_data <- cbind(Data, test_errors_sq.df)
    # Makes a survey design for based off of the whole dataset so we can calculate a mean
    srs.svy <- svydesign(ids = ~0,
                         strata = NULL,
                         fpc = rep(N, nrow(complete_data)),
                         data = complete_data)
    strat.svy <- svydesign(ids = ~0,
                           strata = formula(paste0('~',scID)),
                           fpc = rep(N, nrow(complete_data)),
                           data = complete_data)
    clus.svy <- svydesign(ids = formula(paste0('~',scID)),
                          strata = NULL,
                          fpc = rep(N, nrow(complete_data)),
                          data = complete_data)
    if (error_calc == "SRS") {
      final.svy <- srs.svy
    } else if (error_calc == "Strat") {
      final.svy <- strat.svy
    } else if (error_calc == "Cluster") {
      final.svy <- clus.svy
    }
    # Makes an empty matrix that we can pump the means and SE into for each formula by row
    means <- matrix(NA, nrow=(ncol(complete_data) - ncol(Data)), ncol=2)
    # Sets i for the loop to start at the first column in the dataset that contains test errors sq
    i = ncol(Data) + 1
    # Sets the loop to start on column one of the matrix
    y = 1
    # Makes a data frame of the output from the svymean function and then plugs the Mean output
    # into the first column of the matrix and the SE output into the second column of the matrix
    while (i <= ncol(complete_data)) {
      meansd <- data.frame(svymean(complete_data[,i],final.svy))
      means[y,1] <- meansd$mean
      means[y,2] <- meansd$SE
      i = i+1
      y = y+1
    }
    # Returns the resulting matrix to the console
    return(means)
  }



  # Cross Val for Stratafication and SRS
  cv.strat.srs.lm <- function(Data, formulae, nfolds=5, strataID, N, method = "linear", model = "Strat") {
    # Other option for method is "logistic"

    #stop logic checking dataset specified
    if(nfolds < 1) {print ("nfolds is less that 1")}
    if(N<nrow(Data)) {print("N is less than observations in the defined dataset")}
    if(nfolds >= nrow(Data)) {print ("fold number exceeds observations")}

    stopifnot(nfolds > 0, N >= nrow(Data), nfolds <= nrow(Data))

    # Turns the strings of formulas into a list of formulas
    formulae <- sapply(formulae, as.formula)
    # Pushes the data into this Data data frame

    # Creates an observation ID variable for the dataset
    Data$ID <- 1:nrow(Data)
    # Creates a new dataset that is a copy of Data that we can now scramble
    # and then order by the variable we are stratifying on
    randomized_ds <- data.frame(ID = Data$ID, stratumID = Data[[strataID]])
    randomized_ds <- randomized_ds[sample(1:nrow(randomized_ds)),]
    randomized_ds <- randomized_ds[order(randomized_ds$stratumID),]
    # Assigns a fold ID to to all of the observations
    randomized_ds$foldID <- rep(1:nfolds, length.out=nrow(randomized_ds))
    # Orders the scrambled datset by the observation ID so that this dataset is in orginal order,
    # and then attches the fold ID variable to the Data data frame
    randomized_ds <- randomized_ds[order(randomized_ds$ID),]
    Data$foldID <- randomized_ds$foldID
    # Makes a matrix that the test errors squared will be pumped back into inside the for loop
    test_errors_sq <- matrix(NA, nrow=nrow(Data), ncol=length(formulae))
    # This loops through each fold to create a training dataset and holdout (test) dataset for that
    # k-fold, while also making the svydesign for that fold based on the training dataset
    for (fold in 1:nfolds) {
      test.rows <- which(Data$foldID == fold)
      train <- Data[-test.rows,]
      test <- Data[test.rows,]
      strat.svy <- svydesign(ids = ~0,
                             strata = formula(paste0('~',strataID)),
                             fpc = rep(N, nrow(train)),
                             data = train)
      #
      #
      #
      srs.svy <- svydesign(ids = ~0,
                           strata = NULL,
                           fpc = rep(N, nrow(train)),
                           data = train)
      if (model == "Strat") {
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
      } else if (model == "SRS") {
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
      #
      #
      #
    }
    # This converts our matrix into a data frame so it can more easily manipulated
    test_errors_sq.df <- as.data.frame(test_errors_sq)
    # Attaches our test errors squared back onto the original dataset
    complete_data <- cbind(Data, test_errors_sq.df)
    # Makes a survey design for based off of the whole dataset so we can calculate a mean
    strat.svy <- svydesign(ids = ~0,
                           strata = formula(paste0('~',strataID)),
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
      meansd <- data.frame(svymean(complete_data[,i],strat.svy))
      means[y,1] <- meansd$mean
      means[y,2] <- meansd$SE
      i = i+1
      y = y+1
    }
    # Returns the resulting matrix to the console
    return(means)
  }


  # Cross val for cluster and srs

  cv.cluster.srs.lm <- function(Data, formulae, nfolds=5, clusterID, N, method = "linear", model = "Cluster") {
    # Other option for method is "logistic"

    #stop logic checking dataset specified
    if(nfolds < 1) {print ("nfolds is less that 1")}
    if(N<nrow(Data)) {print("N is less than observations in the defined dataset")}
    if(nfolds >= nrow(Data)) {print ("fold number exceeds observations")}
    if(nfolds > length(unique(Data[[clusterID]]))) {print ("nfolds is larger than the number of clusters")}
    stopifnot(nfolds > 0, N >= nrow(Data), nfolds <= nrow(Data), nfolds <= length(unique(Data[[clusterID]])))

    # Turns the strings of formulas into a list of formulas
    formulae <- sapply(formulae, as.formula)
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
      clus.svy <- svydesign(ids = formula(paste0("~",clusterID)),
                            strata = NULL,
                            fpc = rep(N, nrow(train)),
                            data = train)

      ###

      srs.svy <- svydesign(ids = ~0,
                           strata = NULL,
                           fpc = rep(N, nrow(train)),
                           data = train)

      ###
      # This loops through the formulas in our list of formulas and calculates the test errors
      # squared for thos formulas applied to each survey design made from each fold and plugs
      # those test errors squared back into the matrix we made earlier
      if(model == "Cluster"){
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
      }else if (model == "SRS") {
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

  clusVsrs.cross.cv.spline.plot <- function(n, loops) {
    # Making an empty data set for output when we use SRS samples and make SRS folds
    srssrsds <- data.frame(df = c(), MSE = c())
    # Making an empty data set for output when we use SRS samples and make Cluster folds
    srsclusds <- data.frame(df = c(), MSE = c())
    # Making an empty data set for output when we use Cluster samples and make Cluster folds
    clusclusds <- data.frame(df = c(), MSE = c())
    # Making an empty data set for output when we use Cluster samples and make SRS folds
    clussrsds <- data.frame(df = c(), MSE = c())
    # Making as many simple random samples as we specify for 'loops'
    for (i in 1:loops) {
      set.seed(i)
      sim.srs <- dplyr::sample_n(spline.df2, n)
      # Using our SRS function on SRS samples to get MSE outputs from cross validation using 5 folds
      srs.data <- cv.svy(sim.srs, c("Response~ns(Predictor, df=1)", "Response~ns(Predictor, df=2)",
                                       "Response~ns(Predictor, df=3)", "Response~ns(Predictor, df=4)",
                                       "Response~ns(Predictor, df=5)", "Response~ns(Predictor, df=6)"),
                            nfolds = 5) %>% print()
      # Using our Cluster function on SRS samples to get MSE outputs from cross validation using 5 folds
      clus.data <- cv.svy(sim.srs, c("Response~ns(Predictor, df=1)", "Response~ns(Predictor, df=2)",
                                            "Response~ns(Predictor, df=3)", "Response~ns(Predictor, df=4)",
                                            "Response~ns(Predictor, df=5)", "Response~ns(Predictor, df=6)"),
                                 clusterID = "Cluster", nfolds = 5) %>% print()
      # Taking the outputs from using the SRS function on SRS samples and compiling them into one data frame
      srssrsds2 <- data.frame(df = 1:6, MSE = srs.data[,1])
      srssrsds <- rbind(srssrsds, srssrsds2)
      # Taking the outputs from using the Cluster function on SRS samples and compiling them into one data frame
      srsclusds2 <- data.frame(df = 1:6, MSE = clus.data[,1])
      srsclusds <- rbind(srsclusds, srsclusds2)
    }
    # Making as many cluster samples as we specify for 'loops'
    for (i in 1:loops) {
      set.seed(i)
      c <- unique(spline.df2[["Cluster"]])
      sim.clus <- spline.df2[spline.df2[["Cluster"]] %in% sample(c, n/10),]
      # Using our Cluster function on Cluster samples to get MSE outputs from cross validation using 5 folds
      clus.data <- cv.svy(sim.clus, c("Response~ns(Predictor, df=1)", "Response~ns(Predictor, df=2)",
                                             "Response~ns(Predictor, df=3)", "Response~ns(Predictor, df=4)",
                                             "Response~ns(Predictor, df=5)", "Response~ns(Predictor, df=6)"),
                                 clusterID = "Cluster", nfolds = 5) %>% print()
      # Using our SRS function on Cluster samples to get MSE outputs from cross validation using 5 folds
      srs.data <- cv.svy(sim.clus, c("Response~ns(Predictor, df=1)", "Response~ns(Predictor, df=2)",
                                        "Response~ns(Predictor, df=3)", "Response~ns(Predictor, df=4)",
                                        "Response~ns(Predictor, df=5)", "Response~ns(Predictor, df=6)"),
                            nfolds = 5) %>% print()
      # Taking the outputs from using the Cluster function on Cluster samples and compiling them into one data frame
      clusclusds2 <- data.frame(df = 1:6, MSE = clus.data[,1])
      clusclusds <- rbind(clusclusds, clusclusds2)
      # Taking the outputs from using the SRS function on Cluster samples and compiling them into one data frame
      clussrsds2 <- data.frame(df = 1:6, MSE = srs.data[,1])
      clussrsds <- rbind(clussrsds, clussrsds2)
    }
    # Making the degrees of freedom variable a factor variable for the four different data frames
    srssrsds$df <- as.factor(srssrsds$df)
    srsclusds$df <- as.factor(srsclusds$df)
    clusclusds$df <- as.factor(clusclusds$df)
    clussrsds$df <- as.factor(clussrsds$df)
    # Making a ggplot object for the MSEs where the SRS function was used on SRS samples
    plot1 <- ggplot2::ggplot(data = srssrsds, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::geom_boxplot() +
      ggplot2::ggtitle("SRS folds with SRS sample") +
      ggplot2::ylim(20000, 250000)
    # Making a ggplot object for the MSEs where the Cluster function was used on SRS samples
    plot2 <- ggplot2::ggplot(data = srsclusds, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::geom_boxplot() +
      ggplot2::ggtitle("Cluster folds with SRS sample") +
      ggplot2::ylim(20000, 250000)
    # Making a ggplot object for the MSEs where the Cluster function was used on Cluster samples
    plot3 <- ggplot2::ggplot(data = clusclusds, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::geom_boxplot() +
      ggplot2::ggtitle("Cluster folds with Cluster sample") +
      ggplot2::ylim(20000, 250000)
    # Making a ggplot object for the MSEs where the SRS function was used on Cluster samples
    plot4 <- ggplot2::ggplot(data = clussrsds, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::geom_boxplot() +
      ggplot2::ggtitle("SRS folds with Cluster sample") +
      ggplot2::ylim(20000, 250000)
    # Making a grid display of the four plot objects above
    gridExtra::grid.arrange(plot1,plot2,plot4,plot3, ncol = 2,
                 top = paste0("Simulated Spline Data (Sample Size = ", n, ", Clusters = ", n/10, ", Loops = ", loops, ")"))
  }

  clusVsrs.mixed.cv.spline.plot <- function(n, loops, plot) {
    # Making an empty data set for output when we make SRS folds, use SRS models, and calculate MSEs using SRS design
    srssrssrs <- data.frame(df = c(), MSE = c())
    # Making an empty data set for output when we make SRS folds, use Clus models, and calculate MSEs using SRS design
    srsclussrs <- data.frame(df = c(), MSE = c())
    # Making an empty data set for output when we make Clus folds, use Clus models, and calculate MSEs using Clus design
    clusclusclus <- data.frame(df = c(), MSE = c())
    # Making an empty data set for output when we make Clus folds, use SRS models, and calculate MSEs using Clus design
    clussrsclus <- data.frame(df = c(), MSE = c())
    # Making an empty data set for output when we make SRS folds, use SRS models, and calculate MSEs using Clus design
    srssrsclus <- data.frame(df = c(), MSE = c())
    # Making an empty data set for output when we make SRS folds, use Clus models, and calculate MSEs using Clus design
    srsclusclus <- data.frame(df = c(), MSE = c())
    # Making as many Cluster samples as we specify for 'loops'
    for (i in 1:loops) {
      set.seed(i)
      c <- unique(spline.df2[["Cluster"]])
      sim.clus <- spline.df2[spline.df2[["Cluster"]] %in% sample(c, n/10),]
      # Collecting MSE outputs when using SRS folds, SRS models, and SRS design for error calculations
      srs.data <- cv.srs.mixed.lm(sim.clus, c("Response~ns(Predictor, df=1)", "Response~ns(Predictor, df=2)",
                                              "Response~ns(Predictor, df=3)", "Response~ns(Predictor, df=4)",
                                              "Response~ns(Predictor, df=5)", "Response~ns(Predictor, df=6)"),
                                  scID = "Cluster", nfolds = 5, N = 1000, error_calc = "SRS")
      # Collecting MSE outputs when using SRS folds, Clus models, and SRS design for error calculations
      clus.data <- cv.srs.mixed.lm(sim.clus, c("Response~ns(Predictor, df=1)", "Response~ns(Predictor, df=2)",
                                               "Response~ns(Predictor, df=3)", "Response~ns(Predictor, df=4)",
                                               "Response~ns(Predictor, df=5)", "Response~ns(Predictor, df=6)"),
                                   scID = "Cluster", nfolds = 5, N = 1000, model = "Cluster", error_calc = "SRS")
      # Taking the outputs from using SRS folds, SRS models, and SRS design for error calculations,
      # and compiling them into one data frame
      srssrssrs2 <- data.frame(df = 1:6, MSE = srs.data[,1], sample = rep(i, length.out = 6))
      srssrssrs <- rbind(srssrssrs, srssrssrs2)
      # Taking the outputs from using SRS folds, Clus models, and SRS design for error calculations,
      # and compiling them into one data frame
      srsclussrs2 <- data.frame(df = 1:6, MSE = clus.data[,1], sample = rep(i, length.out = 6))
      srsclussrs <- rbind(srsclussrs, srsclussrs2)
      # Collecting MSE outputs when using Clus folds, Clus models, and Clus design for error calculations
      clus.data <- cv.cluster.srs.lm(sim.clus, c("Response~ns(Predictor, df=1)", "Response~ns(Predictor, df=2)",
                                                 "Response~ns(Predictor, df=3)", "Response~ns(Predictor, df=4)",
                                                 "Response~ns(Predictor, df=5)", "Response~ns(Predictor, df=6)"),
                                     clusterID = "Cluster", nfolds = 5, N = 1000)
      # Collecting MSE outputs when using Clus folds, SRS models, and Clus design for error calculations
      srs.data <- cv.cluster.srs.lm(sim.clus, c("Response~ns(Predictor, df=1)", "Response~ns(Predictor, df=2)",
                                                "Response~ns(Predictor, df=3)", "Response~ns(Predictor, df=4)",
                                                "Response~ns(Predictor, df=5)", "Response~ns(Predictor, df=6)"),
                                    clusterID = "Cluster", nfolds = 5, N = 1000, model = "SRS")
      # Taking the outputs from using Clus folds, Clus models, and Clus design for error calculations,
      # and compiling them into one data frame
      clusclusclus2 <- data.frame(df = 1:6, MSE = clus.data[,1], sample = rep(i, length.out = 6))
      clusclusclus <- rbind(clusclusclus, clusclusclus2)
      # Taking the outputs from using Clus folds, SRS models, and Clus design for error calculations,
      # and compiling them into one data frame
      clussrsclus2 <- data.frame(df = 1:6, MSE = srs.data[,1], sample = rep(i, length.out = 6))
      clussrsclus <- rbind(clussrsclus, clussrsclus2)
      # Collecting MSE outputs when using SRS folds, SRS models, and Clus design for error calculations
      srs.data <- cv.srs.mixed.lm(sim.clus, c("Response~ns(Predictor, df=1)", "Response~ns(Predictor, df=2)",
                                              "Response~ns(Predictor, df=3)", "Response~ns(Predictor, df=4)",
                                              "Response~ns(Predictor, df=5)", "Response~ns(Predictor, df=6)"),
                                  scID = "Cluster", nfolds = 5, N = 1000, error_calc = "Cluster")
      # Collecting MSE outputs when using SRS folds, Clus models, and Clus design for error calculations
      clus.data <- cv.srs.mixed.lm(sim.clus, c("Response~ns(Predictor, df=1)", "Response~ns(Predictor, df=2)",
                                               "Response~ns(Predictor, df=3)", "Response~ns(Predictor, df=4)",
                                               "Response~ns(Predictor, df=5)", "Response~ns(Predictor, df=6)"),
                                   scID = "Cluster", nfolds = 5, N = 1000, model = "Cluster", error_calc = "Cluster")
      # Taking the outputs from using SRS folds, SRS models, and Clus design for error calculations,
      # and compiling them into one data frame
      srssrsclus2 <- data.frame(df = 1:6, MSE = srs.data[,1], sample = rep(i, length.out = 6))
      srssrsclus <- rbind(srssrsclus, srssrsclus2)
      # Taking the outputs from using SRS folds, Clus models, and Clus design for error calculations,
      # and compiling them into one data frame
      srsclusclus2 <- data.frame(df = 1:6, MSE = clus.data[,1], sample = rep(i, length.out = 6))
      srsclusclus <- rbind(srsclusclus, srsclusclus2)
    }
    # Making the degrees of freedom variable a factor variable for the six different data frames
    srssrssrs$df <- as.factor(srssrssrs$df)
    srsclussrs$df <- as.factor(srsclussrs$df)
    clusclusclus$df <- as.factor(clusclusclus$df)
    clussrsclus$df <- as.factor(clussrsclus$df)
    srssrsclus$df <- as.factor(srssrsclus$df)
    srsclusclus$df <- as.factor(srsclusclus$df)
    # Making the sample variable a factor variable for the six different data frames
    srssrssrs$sample <- as.factor(srssrssrs$sample)
    srsclussrs$sample <- as.factor(srsclussrs$sample)
    clusclusclus$sample <- as.factor(clusclusclus$sample)
    clussrsclus$sample <- as.factor(clussrsclus$sample)
    srssrsclus$sample <- as.factor(srssrsclus$sample)
    srsclusclus$sample <- as.factor(srsclusclus$sample)
    # Making a ggplot object for the MSEs collected when using SRS folds, SRS models, and SRS error calculations
    p1 <- ggplot2::ggplot(data = srssrssrs, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::ggtitle("S:Cluster, F:SRS, M:SRS, MSE:SRS") +
      ggplot2::ylim(20000, 250000)
    # Making a ggplot object for the MSEs collected when using SRS folds, Clus models, and SRS error calculations
    p2 <- ggplot2::ggplot(data = srsclussrs, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::ggtitle("S:Cluster, F:SRS, M:Clus, MSE:SRS") +
      ggplot2::ylim(20000, 250000)
    # Making a ggplot object for the MSEs collected when using Clus folds, Clus models, and Clus error calculations
    p3 <- ggplot2::ggplot(data = clusclusclus, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::ggtitle("S:Cluster, F:Clus, M:Clus, MSE:Clus") +
      ggplot2::ylim(20000, 250000)
    # Making a ggplot object for the MSEs collected when using Clus folds, SRS models, and Clus error calculations
    p4 <- ggplot2::ggplot(data = clussrsclus, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::ggtitle("S:Cluster, F:Clus, M:SRS, MSE:Clus") +
      ggplot2::ylim(20000, 250000)
    # Making a ggplot object for the MSEs collected when using SRS folds, SRS models, and Clus error calculations
    p5 <- ggplot2::ggplot(data = srssrsclus, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::ggtitle("S:Cluster, F:SRS, M:SRS, MSE:Clus") +
      ggplot2::ylim(20000, 250000)
    # Making a ggplot object for the MSEs collected when using SRS folds, Clus models, and Clus error calculations
    p6 <- ggplot2::ggplot(data = srsclusclus, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::ggtitle("S:Cluster, F:SRS, M:Clus, MSE:Clus") +
      ggplot2::ylim(20000, 250000)
    # Either turning our ggplot objects into boxplots or spaghetti plots (as objects still)
    if (plot == "box") {
      plot1 <- p1 + ggplot2::geom_boxplot()
      plot2 <- p2 + ggplot2::geom_boxplot()
      plot3 <- p3 + ggplot2::geom_boxplot()
      plot4 <- p4 + ggplot2::geom_boxplot()
      plot5 <- p5 + ggplot2::geom_boxplot()
      plot6 <- p6 + ggplot2::geom_boxplot()
    } else if (plot == "line") {
      plot1 <- p1 + ggplot2::geom_line(ggplot2::aes(group = sample, colour = sample)) + ggplot2::theme(legend.position = "none")
      plot2 <- p2 + ggplot2::geom_line(ggplot2::aes(group = sample, colour = sample)) + ggplot2::theme(legend.position = "none")
      plot3 <- p3 + ggplot2::geom_line(ggplot2::aes(group = sample, colour = sample)) + ggplot2::theme(legend.position = "none")
      plot4 <- p4 + ggplot2::geom_line(ggplot2::aes(group = sample, colour = sample)) + ggplot2::theme(legend.position = "none")
      plot5 <- p5 + ggplot2::geom_line(ggplot2::aes(group = sample, colour = sample)) + ggplot2::theme(legend.position = "none")
      plot6 <- p6 + ggplot2::geom_line(ggplot2::aes(group = sample, colour = sample)) + ggplot2::theme(legend.position = "none")
    }
    # Making a grid display of the six plot objects above
    gridExtra::grid.arrange(plot1,plot2,plot5,plot6,plot4,plot3, ncol = 2,
                 top = paste0("Simulated Spline Data (Sample Size = ", n, ", Clusters = ", n/10,
                              ", Loops = ", loops, ")"))
  }

  stratVsrs.mixed.cv.spline.plot <- function(n, loops, plot) {
    # Making an empty data set for output when we make SRS folds, use SRS models, and calculate MSEs using SRS design
    srssrssrs <- data.frame(df = c(), MSE = c())
    # Making an empty data set for output when we make SRS folds, use Strat models, and calculate MSEs using SRS design
    srsstratsrs <- data.frame(df = c(), MSE = c())
    # Making an empty data set for output when we make Strat folds, use Strat models, and calculate MSEs using Strat design
    stratstratstrat <- data.frame(df = c(), MSE = c())
    # Making an empty data set for output when we make Strat folds, use SRS models, and calculate MSEs using Strat design
    stratsrsstrat <- data.frame(df = c(), MSE = c())
    # Making an empty data set for output when we make SRS folds, use SRS models, and calculate MSEs using Strat design
    srssrsstrat <- data.frame(df = c(), MSE = c())
    # Making an empty data set for output when we make SRS folds, use Strat models, and calculate MSEs using Strat design
    srsstratstrat <- data.frame(df = c(), MSE = c())
    # Making as many Stratification samples as we specify for 'loops'
    for (i in 1:loops) {
      set.seed(i)
      s <- survey::stratsample(spline.df2$Stratum, c("1" = n/5, "2" = n/5, "3" = n/5, "4" = n/5, "5" = n/5))
      sim.strat <- spline.df2[s,]
      # Collecting MSE outputs when using SRS folds, SRS models, and SRS design for error calculations
      srs.data <- cv.srs.mixed.lm(sim.strat, c("Response~ns(Predictor, df=1)", "Response~ns(Predictor, df=2)",
                                               "Response~ns(Predictor, df=3)", "Response~ns(Predictor, df=4)",
                                               "Response~ns(Predictor, df=5)", "Response~ns(Predictor, df=6)"),
                                  scID = "Stratum", nfolds = 5, N = 1000)
      # Collecting MSE outputs when using SRS folds, Strat models, and SRS design for error calculations
      strat.data <- cv.srs.mixed.lm(sim.strat, c("Response~ns(Predictor, df=1)", "Response~ns(Predictor, df=2)",
                                                 "Response~ns(Predictor, df=3)", "Response~ns(Predictor, df=4)",
                                                 "Response~ns(Predictor, df=5)", "Response~ns(Predictor, df=6)"),
                                    scID = "Stratum", nfolds = 5, N = 1000, model = "Strat")
      # Taking the outputs from using SRS folds, SRS models, and SRS design for error calculations,
      # and compiling them into one data frame
      srssrssrs2 <- data.frame(df = 1:6, MSE = srs.data[,1], sample = rep(i, length.out = 6))
      srssrssrs <- rbind(srssrssrs, srssrssrs2)
      # Taking the outputs from using SRS folds, Strat models, and SRS design for error calculations,
      # and compiling them into one data frame
      srsstratsrs2 <- data.frame(df = 1:6, MSE = strat.data[,1], sample = rep(i, length.out = 6))
      srsstratsrs <- rbind(srsstratsrs, srsstratsrs2)
      # Collecting MSE outputs when using Strat folds, Strat models, and Strat design for error calculations
      strat.data <- cv.strat.srs.lm(sim.strat, c("Response~ns(Predictor, df=1)", "Response~ns(Predictor, df=2)",
                                                 "Response~ns(Predictor, df=3)", "Response~ns(Predictor, df=4)",
                                                 "Response~ns(Predictor, df=5)", "Response~ns(Predictor, df=6)"),
                                    strataID = "Stratum", nfolds = 5, N = 1000)
      # Collecting MSE outputs when using Strat folds, SRS models, and Strat design for error calculations
      srs.data <- cv.strat.srs.lm(sim.strat, c("Response~ns(Predictor, df=1)", "Response~ns(Predictor, df=2)",
                                               "Response~ns(Predictor, df=3)", "Response~ns(Predictor, df=4)",
                                               "Response~ns(Predictor, df=5)", "Response~ns(Predictor, df=6)"),
                                  strataID = "Stratum", nfolds = 5, N = 1000, model = "SRS")
      # Taking the outputs from using Strat folds, Strat models, and Strat design for error calculations,
      # and compiling them into one data frame
      stratstratstrat2 <- data.frame(df = 1:6, MSE = strat.data[,1], sample = rep(i, length.out = 6))
      stratstratstrat <- rbind(stratstratstrat, stratstratstrat2)
      # Taking the outputs from using Strat folds, SRS models, and Strat design for error calculations,
      # and compiling them into one data frame
      stratsrsstrat2 <- data.frame(df = 1:6, MSE = srs.data[,1], sample = rep(i, length.out = 6))
      stratsrsstrat <- rbind(stratsrsstrat, stratsrsstrat2)
      # Collecting MSE outputs when using SRS folds, SRS models, and Strat design for error calculations
      srs.data <- cv.srs.mixed.lm(sim.strat, c("Response~ns(Predictor, df=1)", "Response~ns(Predictor, df=2)",
                                               "Response~ns(Predictor, df=3)", "Response~ns(Predictor, df=4)",
                                               "Response~ns(Predictor, df=5)", "Response~ns(Predictor, df=6)"),
                                  scID = "Stratum", nfolds = 5, N = 1000, error_calc = "Strat")
      # Collecting MSE outputs when using SRS folds, Strat models, and Strat design for error calculations
      strat.data <- cv.srs.mixed.lm(sim.strat, c("Response~ns(Predictor, df=1)", "Response~ns(Predictor, df=2)",
                                                 "Response~ns(Predictor, df=3)", "Response~ns(Predictor, df=4)",
                                                 "Response~ns(Predictor, df=5)", "Response~ns(Predictor, df=6)"),
                                    scID = "Stratum", nfolds = 5, N = 1000, model = "Strat", error_calc = "Strat")
      # Taking the outputs from using SRS folds, SRS models, and Strat design for error calculations,
      # and compiling them into one data frame
      srssrsstrat2 <- data.frame(df = 1:6, MSE = srs.data[,1], sample = rep(i, length.out = 6))
      srssrsstrat <- rbind(srssrsstrat, srssrsstrat2)
      # Taking the outputs from using SRS folds, Strat models, and Strat design for error calculations,
      # and compiling them into one data frame
      srsstratstrat2 <- data.frame(df = 1:6, MSE = strat.data[,1], sample = rep(i, length.out = 6))
      srsstratstrat <- rbind(srsstratstrat, srsstratstrat2)
    }
    # Making the degrees of freedom variable a factor variable for the six different data frames
    srssrssrs$df <- as.factor(srssrssrs$df)
    srsstratsrs$df <- as.factor(srsstratsrs$df)
    stratstratstrat$df <- as.factor(stratstratstrat$df)
    stratsrsstrat$df <- as.factor(stratsrsstrat$df)
    srssrsstrat$df <- as.factor(srssrsstrat$df)
    srsstratstrat$df <- as.factor(srsstratstrat$df)
    # Making the sample variable a factor variable for the six different data frames
    srssrssrs$sample <- as.factor(srssrssrs$sample)
    srsstratsrs$sample <- as.factor(srsstratsrs$sample)
    stratstratstrat$sample <- as.factor(stratstratstrat$sample)
    stratsrsstrat$sample <- as.factor(stratsrsstrat$sample)
    srssrsstrat$sample <- as.factor(srssrsstrat$sample)
    srsstratstrat$sample <- as.factor(srsstratstrat$sample)
    # Making a ggplot object for the MSEs collected when using SRS folds, SRS models, and SRS error calculations
    p1 <- ggplot2::ggplot(data = srssrssrs, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::ggtitle("S:Strat, F:SRS, M:SRS, MSE:SRS") +
      ggplot2::ylim(20000, 110000)
    # Making a ggplot object for the MSEs collected when using SRS folds, Strat models, and SRS error calculations
    p2 <- ggplot2::ggplot(data = srsstratsrs, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::ggtitle("S:Strat, F:SRS, M:Strat, MSE:SRS") +
      ggplot2::ylim(20000, 110000)
    # Making a ggplot object for the MSEs collected when using Strat folds, Strat models, and Strat error calculations
    p3 <- ggplot2::ggplot(data = stratstratstrat, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::ggtitle("S:Strat, F:Strat, M:Strat, MSE:Strat") +
      ggplot2::ylim(20000, 110000)
    # Making a ggplot object for the MSEs collected when using Strat folds, SRS models, and Strat error calculations
    p4 <- ggplot2::ggplot(data = stratsrsstrat, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::ggtitle("S:Strat, F:Strat, M:SRS, MSE:Strat") +
      ggplot2::ylim(20000, 110000)
    # Making a ggplot object for the MSEs collected when using SRS folds, SRS models, and Strat error calculations
    p5 <- ggplot2::ggplot(data = srssrsstrat, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::ggtitle("S:Strat, F:SRS, M:SRS, MSE:Strat") +
      ggplot2::ylim(20000, 110000)
    # Making a ggplot object for the MSEs collected when using SRS folds, Strat models, and Strat error calculations
    p6 <- ggplot2::ggplot(data = srsstratstrat, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::ggtitle("S:Strat, F:SRS, M:Strat, MSE:Strat") +
      ggplot2::ylim(20000, 110000)
    # Either turning our ggplot objects into boxplots or spaghetti plots (as objects still)
    if (plot == "box") {
      plot1 <- p1 + ggplot2::geom_boxplot()
      plot2 <- p2 + ggplot2::geom_boxplot()
      plot3 <- p3 + ggplot2::geom_boxplot()
      plot4 <- p4 + ggplot2::geom_boxplot()
      plot5 <- p5 + ggplot2::geom_boxplot()
      plot6 <- p6 + ggplot2::geom_boxplot()
    } else if (plot == "line") {
      plot1 <- p1 + ggplot2::geom_line(ggplot2::aes(group = sample, colour = sample)) + ggplot2::theme(legend.position = "none")
      plot2 <- p2 + ggplot2::geom_line(ggplot2::aes(group = sample, colour = sample)) + ggplot2::theme(legend.position = "none")
      plot3 <- p3 + ggplot2::geom_line(ggplot2::aes(group = sample, colour = sample)) + ggplot2::theme(legend.position = "none")
      plot4 <- p4 + ggplot2::geom_line(ggplot2::aes(group = sample, colour = sample)) + ggplot2::theme(legend.position = "none")
      plot5 <- p5 + ggplot2::geom_line(ggplot2::aes(group = sample, colour = sample)) + ggplot2::theme(legend.position = "none")
      plot6 <- p6 + ggplot2::geom_line(ggplot2::aes(group = sample, colour = sample)) + ggplot2::theme(legend.position = "none")
    }
    # Making a grid display of the six plot objects above
    gridExtra::grid.arrange(plot1,plot2,plot5,plot6,plot4,plot3, ncol = 2,
                 top = paste0("Simulated Spline Data (Sample Size = ", n,
                              ", Strata = 5, Loops = ", loops, ")"))
  }

  sim.clusvsrs.sd.n100 <- clusVsrs.cross.cv.spline.plot(n=100, loops=100)
  sim.clusvsrs.bp.n100 <- clusVsrs.mixed.cv.spline.plot(n=100, loops=100, plot = "box")
  sim.stratvsrs.bp.n100 <- stratVsrs.mixed.cv.spline.plot(n=100, loops=100, plot = "box")

  return(list(sim.clusvsrs.sd.n100 = sim.clusvsrs.sd.n100,
              sim.clusvsrs.bp.n100 = sim.clusvsrs.bp.n100,
              sim.stratvsrs.bp.n100 = sim.stratvsrs.bp.n100))
}

plot_generation2 <- function() {

  # Simulating Splined data

  set.seed(47)
  x1 = stats::runif(1:500, min = 2, max = 14)
  y1 = x1^3 - 17*x1^2 + 10*x1 + 1000


  set.seed(47)
  x2 = stats::runif(1:500, min = 14, max = 26)
  y2 = -1*(x2-12)^3 + 14*(x2-12)^2 + 15*(x2-12) + 500
  plot(y2~x2)

  set.seed(47)
  x3 = stats::runif(1:500, min = 26, max = 38)
  y3 = (x3-29)^3 - 13*(x3-29)^2 + 0*(x3-29) + 900


  set.seed(47)
  x4 = stats::runif(1:500, min = 38, max = 50)
  y4 = (x4-36)^3 - 10*(x4-36)^2 + 2*(x4-36) + 600


  set.seed(47)
  z1 = jitter(y1, 15000)
  z1 = jitter(y1, 15000)
  z2 = jitter(y2, 15000)
  z3 = jitter(y3, 15000)
  z4 = jitter(y4, 15000)

  ds1 <- data.frame(Response = z1, Predictor = x1)
  ds2 <- data.frame(Response = z2, Predictor = x2)
  ds3 <- data.frame(Response = z3, Predictor = x3)
  ds4 <- data.frame(Response = z4, Predictor = x4)

  ds <- rbind(ds3, ds4)


  b <- data.frame(ID = c(1:1000))
  spline.df2 <- cbind(b, ds)
  spline.df2 <- spline.df2 %>%
    dplyr::arrange(Predictor) %>%
    dplyr::mutate(Stratum = dplyr::row_number(),
                  Cluster = dplyr::row_number())
  spline.df2$Stratum <- cut(spline.df2$Stratum,5, 1:5)
  spline.df2$Cluster <- cut(spline.df2$Cluster,100, 1:100)
  spline.df2 <- spline.df2 %>%
    dplyr::arrange(ID) %>%
    dplyr::select(ID, Response, Predictor, Cluster, Stratum)

  # Adding weights

  # samp_prob = log(y)/sum(log(y))

  ysum = sum(log(spline.df2$Response))

  spline.df3 <- spline.df2 %>%
    dplyr::mutate(samp_prob = log(Response)/sum(log(spline.df2$Response)))



  # Try some other ways to generate sampling probs:

  # Maybe log(Resp)/sum(log(Resp)) just didn't provide enough spread?
  # The ratio of max to min samp probs was around log(1600) to log(500) or around 1.2,
  # so largest prob was only 20% more than smallest.
  # Let's try Resp/sum(Resp) instead,
  # where largest ratio would be around 1600/500 = 3.2.
  spline.df3$samp_prob_rawratio <- with(spline.df3, Response / sum(Response))
  sum(spline.df3$samp_prob_rawratio)



  # But even so, the sampling probs for highest y-values
  # are are pretty similar across range of x,
  # and same for samp probs for lowest y-values.
  # So by ignoring samp probs, we might fit a spline shifted up,
  # but we'll still fit the same approx shape as unweighted,
  # so won't see much difference in CV's choice of spline df.
  # Maybe instead, we need to oversample points
  # that are close to a POOR FIT to the data?
  # Let's try fitting a quadratic (clearly bad fit)...

  # ...then oversample points NEAR THIS LINE,
  # so that unweighted fit should be close to this quadratic,
  # while weighted fit should be closer to correct high-df spline.
  lm_quad <- stats::lm(Response ~ Predictor + I(Predictor^2), data = spline.df3)
  spline.df3$samp_prob_quad <- (1/(abs(lm_quad$residuals))) / sum(1/(abs(lm_quad$residuals)))

  quad.sample.graph <- ggplot2::ggplot(spline.df3, ggplot2::aes(x = Predictor, y = Response, size = samp_prob_quad)) +
    ggplot2::geom_point(alpha = 0.2) +
    ggplot2::stat_smooth(method = "lm", formula = y ~ x + I(x^2))

  # Try the same thing but with a linear fit -- even simpler
  lm_lin <- stats::lm(Response ~ Predictor, data = spline.df3)
  spline.df3$samp_prob_lin <- (1/(abs(lm_lin$residuals))) / sum(1/(abs(lm_lin$residuals)))



  spline.df3$samp_wt <- 1/spline.df3$samp_prob
  spline.df3$samp_wt_rawratio <- 1/spline.df3$samp_prob_rawratio
  spline.df3$samp_wt_lin <- 1/spline.df3$samp_prob_lin
  spline.df3$samp_wt_quad <- 1/spline.df3$samp_prob_quad



  cv.srs.lm1 <- function(Data, formulae, nfolds=5, N, method = "linear", weights =  NULL) {
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
      srs.svy <- svydesign(ids = ~0,
                           strata = NULL,
                           fpc = rep(N, nrow(train)),
                           weights = if(is.null(weights)) NULL else formula(paste0("~", weights)),
                           data = train)
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
    srs.svy <- svydesign(ids = ~0,
                         strata = NULL,
                         fpc = rep(N, nrow(complete_data)),
                         weights = if(is.null(weights)) NULL else formula(paste0("~", weights)),
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
      meansd <- data.frame(svymean(complete_data[,i],srs.svy))
      means[y,1] <- meansd$mean
      means[y,2] <- meansd$SE
      i = i+1
      y = y+1
    }
    # Returns the resulting matrix to the console
    return(means)
  }


  cv.srs.lm2 <- function(Data, formulae, nfolds=5, N, method = "linear", weights =  NULL) {
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
      srs.svy <- svydesign(ids = ~0,
                           strata = NULL,
                           fpc = rep(N, nrow(train)),
                           weights = formula(paste0("~",weights)),
                           data = train)
      # This loops through the formulas in our list of formulas and calculates the test errors
      # squared for those formulas applied to each survey design made from each fold and plugs
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
    srs.svy <- svydesign(ids = ~0,
                         strata = NULL,
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
      meansd <- data.frame(svymean(complete_data[,i],srs.svy))
      means[y,1] <- meansd$mean
      means[y,2] <- meansd$SE
      i = i+1
      y = y+1
    }
    # Returns the resulting matrix to the console
    return(means)
  }

  cv.srs.lm3 <- function(Data, formulae, nfolds=5, N, method = "linear", weights =  NULL) {
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
      srs.svy <- svydesign(ids = ~0,
                           strata = NULL,
                           fpc = rep(N, nrow(train)),
                           data = train)
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
    srs.svy <- svydesign(ids = ~0,
                         strata = NULL,
                         fpc = rep(N, nrow(complete_data)),
                         weights = formula(paste0("~",weights)),
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
      meansd <- data.frame(svymean(complete_data[,i],srs.svy))
      means[y,1] <- meansd$mean
      means[y,2] <- meansd$SE
      i = i+1
      y = y+1
    }
    # Returns the resulting matrix to the console
    return(means)
  }


  #CV1 uses weights for both MSE and model generation, CV2 uses  it when modeling but not during MSE generation and CV3 uses it only during MSE generation

  SRS.Weight.plot <- function(n, loops, plot, weights) {
    # Making an empty data set for output when we make SRS folds, use SRS models, and calculate MSEs using SRS design
    AllW <- data.frame(df = c(), MSE = c())
    # Making an empty data set for output when we make SRS folds, use Clus models, and calculate MSEs using SRS design
    NoW <- data.frame(df = c(), MSE = c())
    # Making an empty data set for output when we make Clus folds, use Clus models, and calculate MSEs using Clus design
    ModW <- data.frame(df = c(), MSE = c())
    # Making an empty data set for output when we make Clus folds, use SRS models, and calculate MSEs using Clus design
    MSEW <- data.frame(df = c(), MSE = c())
    for (i in 1:loops) {
      # Take a sample of size n, using the sampling probabilities instead of SRS
      # (using 1/samp_wt as the samp_prob)
      stopifnot(all.equal(sum(1/spline.df3[[weights]]), 1))
      in.sample <- sampling::UPtille(n / spline.df3[[weights]])
      spline.df3.sample <- spline.df3[in.sample > 0, ]
      # Collecting MSE outputs when using SRS folds, SRS models, and SRS design for error calculations
      AllWdat <- cv.srs.lm1(spline.df3.sample, c("Response~ns(Predictor, df=1)", "Response~ns(Predictor, df=2)",
                                                 "Response~ns(Predictor, df=3)", "Response~ns(Predictor, df=4)",
                                                 "Response~ns(Predictor, df=5)", "Response~ns(Predictor, df=6)"),
                            nfolds = 5, N = 1000, weights = weights)
      NoWdat <- cv.srs.lm1(spline.df3.sample, c("Response~ns(Predictor, df=1)", "Response~ns(Predictor, df=2)",
                                                "Response~ns(Predictor, df=3)", "Response~ns(Predictor, df=4)",
                                                "Response~ns(Predictor, df=5)", "Response~ns(Predictor, df=6)"),
                           nfolds = 5, N = 1000)
      ModWdat <- cv.srs.lm2(spline.df3.sample, c("Response~ns(Predictor, df=1)", "Response~ns(Predictor, df=2)",
                                                 "Response~ns(Predictor, df=3)", "Response~ns(Predictor, df=4)",
                                                 "Response~ns(Predictor, df=5)", "Response~ns(Predictor, df=6)"),
                            nfolds = 5, N = 1000, weights = weights)
      MSEWdat <- cv.srs.lm3(spline.df3.sample, c("Response~ns(Predictor, df=1)", "Response~ns(Predictor, df=2)",
                                                 "Response~ns(Predictor, df=3)", "Response~ns(Predictor, df=4)",
                                                 "Response~ns(Predictor, df=5)", "Response~ns(Predictor, df=6)"),
                            nfolds = 5, N = 1000, weights = weights)

      # compiling one data frame
      AllW2 <- data.frame(df = 1:6, MSE = AllWdat[,1], sample = rep(i, length.out = 6))
      AllW <- rbind(AllW, AllW2)
      NoW2 <- data.frame(df = 1:6, MSE = NoWdat[,1], sample = rep(i, length.out = 6))
      NoW <- rbind(NoW, NoW2)
      ModW2 <- data.frame(df = 1:6, MSE = ModWdat[,1], sample = rep(i, length.out = 6))
      ModW <- rbind(ModW, ModW2)
      MSEW2 <- data.frame(df = 1:6, MSE = MSEWdat[,1], sample = rep(i, length.out = 6))
      MSEW <- rbind(MSEW, MSEW2)
    }
    # Making the degrees of freedom variable a factor variable for the six different data frames
    AllW$df <- as.factor(AllW$df)
    NoW$df <- as.factor(NoW$df)
    MSEW$df <- as.factor(MSEW$df)
    ModW$df <- as.factor(ModW$df)
    # Making the sample variable a factor variable for the six different data frames
    AllW$sample <- as.factor(AllW$sample)
    NoW$sample <- as.factor(NoW$sample)
    MSEW$sample <- as.factor(MSEW$sample)
    ModW$sample <- as.factor(ModW$sample)
    # Find the y-range of MSEs
    ymin <- min(min(AllW$MSE), min(NoW$MSE), min(MSEW$MSE), min(ModW$MSE))
    ymax <- max(max(AllW$MSE), max(NoW$MSE), max(MSEW$MSE), max(ModW$MSE))
    # Making a ggplot object for the MSEs collected when using SRS folds, SRS models, and SRS error calculations
    p1 <- ggplot2::ggplot(data = AllW, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::ggtitle("Weights for both") +
      ggplot2::ylim(ymin, ymax)
    # Making a ggplot object for the MSEs collected when using SRS folds, Clus models, and SRS error calculations
    p2 <- ggplot2::ggplot(data = NoW, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::ggtitle("No Weights") +
      ggplot2::ylim(ymin, ymax)
    # Making a ggplot object for the MSEs collected when using Clus folds, Clus models, and Clus error calculations
    p3 <- ggplot2::ggplot(data = ModW, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::ggtitle("Weights when modeling") +
      ggplot2::ylim(ymin, ymax)
    # Making a ggplot object for the MSEs collected when using Clus folds, SRS models, and Clus error calculations
    p4 <- ggplot2::ggplot(data = MSEW, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::ggtitle("Weights when MSE gen") +
      ggplot2::ylim(ymin, ymax)

    # Either turning our ggplot objects into boxplots or spaghetti plots (as objects still)
    if (plot == "box") {
      plot1 <- p1 + ggplot2::geom_boxplot()
      plot2 <- p2 + ggplot2::geom_boxplot()
      plot3 <- p3 + ggplot2::geom_boxplot()
      plot4 <- p4 + ggplot2::geom_boxplot()
    } else if (plot == "line") {
      plot1 <- p1 + ggplot2::geom_line(ggplot2::aes(group = sample, colour = sample)) + ggplot2::theme(legend.position = "none")
      plot2 <- p2 + ggplot2::geom_line(ggplot2::aes(group = sample, colour = sample)) + ggplot2::theme(legend.position = "none")
      plot3 <- p3 + ggplot2::geom_line(ggplot2::aes(group = sample, colour = sample)) + ggplot2::theme(legend.position = "none")
      plot4 <- p4 + ggplot2::geom_line(ggplot2::aes(group = sample, colour = sample)) + ggplot2::theme(legend.position = "none")
    }
    # Making a grid display of the six plot objects above
    gridExtra::grid.arrange(plot1,plot2,plot3,plot4, ncol = 2,
                 top = paste0("Simulated Spline Data (Sample Size = ", n,
                              ", Loops = ", loops, ", Weights = ", weights, ")"))
  }


  Main.weights.plot <- SRS.Weight.plot(200,10,"box","samp_wt_quad")







  # checking what individual weighted samples may look like.
  # for samp_wt_quad, we'd expect most of the sampled points
  #   to fall near the quadratic-fit line,
  #   even if doesn't match the general trend of full dataset.
  in.sample.tmp <- sampling::UPtille(100 / spline.df3[["samp_wt_quad"]])
  sum(in.sample.tmp > 0)
  spline.df3.sample.tmp <- spline.df3[in.sample.tmp > 0, ]
  ggplot2::ggplot(spline.df3.sample.tmp, ggplot2::aes(x = Predictor, y = Response, size = samp_prob_quad, alpha = samp_prob_quad)) +
    ggplot2::geom_point()
  ggplot2::ggplot(spline.df3, ggplot2::aes(x = Predictor, y = Response, size = samp_prob_quad, alpha = samp_prob_quad)) +
    ggplot2::geom_point()

  return(list(Main.weights.plot = Main.weights.plot, quad.sample.graph = quad.sample.graph))
}

plot_generation3 <- function(){

  # Using NSFG data at the pregancy level, same as Hunter Ratliff,
  # like we originally did
  # (though later realized it would be better to do this at respondent level,
  #  NOT at this level where there may be multiple pregnancies per respondent...
  #  TODO: we ought to redo this eventually)
  NSFG_data_everypreg$strata <- as.factor(NSFG_data_everypreg$strata)
  NSFG_data_everypreg$SECU <- as.factor(NSFG_data_everypreg$SECU)

  income.year_edu.plot <- function(loops) {
    # Making an empty data set for output when we take into consideration the design method
    method.ds <- data.frame(MSE = c())
    # Making an empty data set for output when we ignore the design method
    ignore.ds <- data.frame(MSE = c())
    fold.ds <- data.frame(MSE = c())
    # looping and generating MSEs
    for (i in 1:loops) {
      set.seed(i)
      method.data <- cv.svy(NSFG_data_everypreg, c("income~ns(YrEdu, df = 1)","income~ns(YrEdu, df = 2)","income~ns(YrEdu, df = 3)","income~ns(YrEdu, df = 4)","income~ns(YrEdu, df = 5)","income~ns(YrEdu, df = 6)"),
                            nfolds = 4, strataID = "strata",
                            clusterID = "SECU", nest = TRUE, weightsID = "wgt") %>% print()
      method.ds2 <- data.frame(df = 1:6, MSE = method.data[,1])
      method.ds <- rbind(method.ds, method.ds2)
      ignore.data <- cv.svy(NSFG_data_everypreg, c("income~ns(YrEdu, df = 1)","income~ns(YrEdu, df = 2)","income~ns(YrEdu, df = 3)","income~ns(YrEdu, df = 4)","income~ns(YrEdu, df = 5)","income~ns(YrEdu, df = 6)"),
                            nfolds = 4) %>% print()
      ignore.ds2 <- data.frame(df = 1:6, MSE = ignore.data[,1])
      ignore.ds <- rbind(ignore.ds, ignore.ds2)
      fold.data <- cv.svy(NSFG_data_everypreg, c("income~ns(YrEdu, df = 1)","income~ns(YrEdu, df = 2)","income~ns(YrEdu, df = 3)","income~ns(YrEdu, df = 4)","income~ns(YrEdu, df = 5)","income~ns(YrEdu, df = 6)"),
                          nfolds = 4, strataID = "strata",
                          clusterID = "SECU", nest = TRUE, weightsID = "wgt", useSvyForFolds = FALSE) %>% print()
      fold.ds2 <- data.frame(df = 1:6, MSE = fold.data[,1])
      fold.ds <- rbind(fold.ds, fold.ds2)
      }
    ignore.ds$df <- as.factor(ignore.ds$df)
    method.ds$df <- as.factor(method.ds$df)
    fold.ds$df <- as.factor(fold.ds$df)
    # Making a ggplot object for the MSE spread comparison
    plot1 <- ggplot2::ggplot(data = ignore.ds, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::geom_boxplot() +
      ggplot2::ggtitle("Ignoring Design") +
      ggplot2::ylim(18000, 19000)
    plot2 <- ggplot2::ggplot(data = fold.ds, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::geom_boxplot() +
      ggplot2::ggtitle("Ignoring Design for Folds") +
      ggplot2::ylim(18000, 19000)
    plot3 <- ggplot2::ggplot(data = method.ds, mapping = ggplot2::aes(x = df, y = MSE)) +
      ggplot2::geom_boxplot() +
      ggplot2::ggtitle("Accounting for Design") +
      ggplot2::ylim(18000, 19000)

    # Making a grid display of the two plot objects above
    gridExtra::grid.arrange(plot1,plot2,plot3, ncol = 3)
  }

  NSFG.plot <- income.year_edu.plot(100)

  NSFG.cluster.plot <- ggplot2::ggplot(NSFG_data_everypreg, ggplot2::aes(x = YrEdu, y = income)) +
    ggplot2::geom_jitter(pch='.') +
    ggplot2::geom_smooth(method = "loess", se = TRUE) +
    ggplot2::facet_grid(strata~SECU) +  ## SECUs are nested within strata
    ggplot2::labs(x = "Years Educated", y = "Income (Expressed as % of Poverty Level)",
         title = "Relationship Separated by Cluster")

  NSFG.strata.plot <- ggplot2::ggplot(NSFG_data_everypreg, ggplot2::aes(x = YrEdu, y = income)) +
    ggplot2::geom_jitter() +
    ggplot2::geom_smooth(method = "loess", se = TRUE) +
    ggplot2::facet_wrap(strata~., ncol = 6) +
    ggplot2::labs(x = "Years Educated", y = "Income (Expressed as % of Poverty Level)",
         title = "Relationship Separated by Stratum")

  return(list(NSFG.plot = NSFG.plot, NSFG.cluster.plot = NSFG.cluster.plot, NSFG.strata.plot = NSFG.strata.plot))
}
