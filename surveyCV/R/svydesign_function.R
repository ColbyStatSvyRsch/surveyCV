#' CV Survey Design function
#'
#' This function combines the other cross validation functions in this package
#' into a single function that is able to interpret datasets and also
#' survey design objects and survey glms
#'
#' @param formulae Vector of formulas containing the variables to be compared in
#'   cross validation
#' @param nfolds Number of folds to be used during cross validation, defaults to
#'   5
#' @param clusterID String of the variable used to cluster during sampling, must
#'   be the same as in the dataset used, ignore if clustering was not used
#' @param strataID String of the variable used to stratify during sampling, must
#'   be the same as in the dataset used, ignore if stratification was not used
#' @param design_object Name of a Survey Design Object created using the Survey
#'   package if one is being used, defaults to NULL
#' @param N Number equal to the total population size
#' @param method string, must be either linear or logistic, determines type of
#'   model fit during cross validation, defaults to linear
#' @param weights Variable in data set that contains PPS weights
#'   @examples
#' #MSEs generated for different tests of first and second degree polynomial
#' # fits predicting mpg from horsepower in the Auto Dataset. Clustering and
#' Stratification was done along the "year" variable.
#' #Using a survey design object to generate MSEs.
#' data("Auto")
#' auto.srs.svy <- svydesign(ids = ~0,
#'                          strata = NULL,
#'                          fpc = rep(1000, 392),
#'                          data = Auto)
#' auto.clus.svy <- svydesign(ids = ~year,
#'                           strata = NULL,
#'                           fpc = rep(1000, 392),
#'                           data = Auto)
#' auto.strat.svy <- svydesign(ids = ~0,
#'                            strata = ~year,
#'                            fpc = rep(1000, 392),
#'                            data = Auto)
#' cv.svy.glm(formulae = c("mpg~poly(horsepower,1, raw = TRUE)", "mpg~poly(horsepower,2, raw = TRUE)",
#'                        "mpg~poly(horsepower,3, raw = TRUE)"), design_object = auto.srs.svy, nfolds = 10, N = 1000)
#' cv.svy.glm(formulae = c("mpg~poly(horsepower,1, raw = TRUE)", "mpg~poly(horsepower,2, raw = TRUE)",
#'                        "mpg~poly(horsepower,3, raw = TRUE)"), design_object = auto.clus.svy, nfolds = 10, N = 1000)
#' cv.svy.glm(formulae = c("mpg~poly(horsepower,1, raw = TRUE)", "mpg~poly(horsepower,2, raw = TRUE)",
#'                        "mpg~poly(horsepower,3, raw = TRUE)"), design_object = auto.strat.svy, nfolds = 10, N = 1000)
#' @export



cv.svydesign <- function(formulae, nfolds=5, N, clusterID = NULL, strataID = NULL, method = "linear", weights = NULL,
                      design_object = NULL) {
  # When a survey design object is specified, then the function can pull pieces of information needed from the design object
  if (is.null(design_object) == FALSE) {
    .data <- design_object[["variables"]]
    cluster_var = gsub("~", "", paste0(design_object[["call"]][["ids"]]))
    # Creates a clusterID variable if it is present in the design object
    if (cluster_var[2] != "0" & cluster_var[2] != "1") {
      sample_type = "Cluster"
      clusterID = cluster_var[2]
    }
    # Creates a strataID variable if it is present in the design object
    if (design_object[["has.strata"]] == TRUE) {
      sample_type = "Strat"
      strataID = paste0(design_object[["call"]][["strata"]][[2]])
    }
    # Makes a nest = TRUE for later use if needed
    if (is.null(design_object[["call"]][["nest"]]) == TRUE) {
      nest = FALSE
    } else if (design_object[["call"]][["nest"]] == TRUE) {
      nest = TRUE
    } else {nest = FALSE}
    # Makes a weights variable if present in the design object
    if (is.null(design_object[["call"]][["weights"]]) == FALSE) {
      weights = paste0(design_object[["call"]][["weights"]][[2]])
    } else {weights = FALSE}
    
  }
  # Runs our general cv.svy() function with all of the variables and data pulled from the design object
  cv.svy(Data = .data, formulae = formulae, nfolds = nfolds, clusterID = clusterID, strataID = strataID, N = N, method = method, 
            weights = weights, nest = nest)
  
}

