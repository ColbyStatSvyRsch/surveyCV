#' CV Survey function
#'
#' This function combines the other cross validation functions in this package
#' into a single function that is able to interpret datasets and also
#' survey design objects and survey glms
#'
#' @param Data CSV of dataset to be tested
#' @param formulae Vector of formulas containing the variables to be compared in
#'   cross validation
#' @param nfolds Number of folds to be used during cross validation, defaults to
#'   5
#' @param clusterID String of the variable used to cluster during sampling, must
#'   be the same as in the dataset used, ignore if clustering was not used
#' @param strataID String of the variable used to stratify during sampling, must
#'   be the same as in the dataset used, ignore if stratification was not used
#' @param sample_type string, must be either SRS, Cluster, or Strat, specifies
#'   which sampling method was used, defaults to SRS
#' @param design_object Name of a Survey Design Object created using the Survey
#'   package if one is being used, defaults to NULL
#' @param glm Name of a Survey GLM created from the survey package if one is
#'   to be used, defaults to NULL
#' @param N Number equal to the total population size
#' @param method string, must be either linear or logistic, determines type of
#'   model fit during cross validation, defaults to linear
#' @param weights Variable in data set that contains PPS weights
#'   @examples
#' #MSEs generated for different tests of first and second degree polynomial
#' # fits predicting mpg from horsepower in the Auto Dataset. Clustering and
#' Stratification were done along the was done along the "year" variable
#' data("Auto")
#' cv.svy.glm(Auto, c("mpg~poly(horsepower,1, raw = TRUE)", "mpg~poly(horsepower,2, raw = TRUE)"), nfolds = 10, N = 400)
#' cv.svy.glm(Auto, c("mpg~poly(horsepower,1, raw = TRUE)", "mpg~poly(horsepower,2, raw = TRUE)"), nfolds = 10, N = 400, clusterID = "year", sample_type = "Cluster")
#' cv.svy.glm(Auto, c("mpg~poly(horsepower,1, raw = TRUE)", "mpg~poly(horsepower,2, raw = TRUE)"), nfolds = 10, N = 400, strataID = "year", sample_type = "Strat")
#' #Using either a survey design object or survey glm to generate MSEs
#' data("Auto")
#' auto.srs.svy <- svydesign(ids = ~0,
#'                          strata = NULL,
#'                          fpc = rep(1000, 392),
#'                          data = Auto)
#' srs.model <- svyglm(mpg~horsepower+I(horsepower^2)+I(horsepower^3), design = auto.srs.svy)
#' auto.clus.svy <- svydesign(ids = ~year,
#'                           strata = NULL,
#'                           fpc = rep(1000, 392),
#'                           data = Auto)
#' clus.model <- svyglm(mpg~horsepower+I(horsepower^2)+I(horsepower^3), design = auto.clus.svy)
#' auto.strat.svy <- svydesign(ids = ~0,
#'                            strata = ~year,
#'                            fpc = rep(1000, 392),
#'                            data = Auto)
#' strat.model <- svyglm(mpg~horsepower+I(horsepower^2)+I(horsepower^3), design = auto.strat.svy)
#' cv.svy.glm(formulae = c("mpg~poly(horsepower,1, raw = TRUE)", "mpg~poly(horsepower,2, raw = TRUE)",
#'                        "mpg~poly(horsepower,3, raw = TRUE)"), design_object = auto.srs.svy, nfolds = 10, N = 1000)
#' cv.svy.glm(formulae = c("mpg~poly(horsepower,1, raw = TRUE)", "mpg~poly(horsepower,2, raw = TRUE)",
#'                        "mpg~poly(horsepower,3, raw = TRUE)"), design_object = auto.clus.svy, nfolds = 10, N = 1000)
#' cv.svy.glm(formulae = c("mpg~poly(horsepower,1, raw = TRUE)", "mpg~poly(horsepower,2, raw = TRUE)",
#'                        "mpg~poly(horsepower,3, raw = TRUE)"), design_object = auto.strat.svy, nfolds = 10, N = 1000)
#' cv.svy.glm(glm = srs.model, nfolds = 10, N = 1000)
#' cv.svy.glm(glm = clus.model, nfolds = 10, N = 1000)
#' cv.svy.glm(glm = strat.model, nfolds = 10, N = 1000)
#' @export



cv.svy.glm <- function(Data, formulae, nfolds=5, N, clusterID, strataID, method = "linear", sample_type = "SRS", weights = NULL,
                       design_object = NULL, glm = NULL) {

  if (is.null(glm) == FALSE) {
    model = glm
    b <- paste0(model[["formula"]][[2]])
    c <- paste0(model[["formula"]][[3]])

    if (length(b) == 2) {

      b1 <- b[1]
      b2 <- b[2]
      ys <- paste0(b1, "(", b2, ")")

      if (length(c) == 3) {
        c1 <- c[1]
        c2 <- c[2]
        c3 <- c[3]

        if (c1 == "ns") {
          xs <- paste0(c1, "(", c2, ",df=", c3, ")")
        } else if (c1 == "poly") {
          xs <- paste0(c1, "(", c2, ",raw=", c3, ")")
        } else {xs <- paste0(c2, c1, c3)}

      } else if (length(c) == 2) {
        c1 <- c[1]
        c2 <- c[2]
        xs <- paste0(c1, "(", c2, ")")
      } else if (length(c) == 1) {
        xs <- c
      }

      formulae = paste0(ys, "~", xs)

    } else if (length(b) == 1) {

      if (length(c) == 3) {
        c1 <- c[1]
        c2 <- c[2]
        c3 <- c[3]

        if (c1 == "ns") {
          xs <- paste0(c1, "(", c2, ",df=", c3, ")")
        } else if (c1 == "poly") {
          xs <- paste0(c1, "(", c2, ",raw=", c3, ")")
        } else {xs <- paste0(c2, c1, c3)}

      } else if (length(c) == 2) {
        c1 <- c[1]
        c2 <- c[2]
        xs <- paste0(c1, "(", c2, ")")
      } else if (length(c) == 1) {
        xs <- c
      }

      formulae = paste0(model[["formula"]][[2]], "~", xs)

    }

    design_object = model[["survey.design"]]

  }

  if (is.null(design_object) == FALSE) {
    Data = cbind(design_object[["variables"]], design_object[["cluster"]], design_object[["strata"]])
    cluster_var = gsub("~", "", paste0(design_object[["call"]][["ids"]]))

    if (cluster_var[2] != "0" & cluster_var[2] != "1") {
      sample_type = "Cluster"
      clusterID = cluster_var[2]
    }

    if (design_object[["has.strata"]] == TRUE) {
      sample_type = "Strat"
      strataID = paste0(design_object[["call"]][["strata"]][[2]])
    }

  }

  if (sample_type == "SRS") {
    cv.srs.lm(Data, formulae, nfolds = nfolds, N = N, method = method, weights = weights)
  } else if (sample_type == "Cluster") {
    cv.cluster.lm(Data, formulae, nfolds = nfolds, clusterID = clusterID, N = N, method = method, weights = weights)
  } else if (sample_type == "Strat") {
    cv.strat.lm(Data, formulae, nfolds = nfolds, strataID = strataID, N = N, method = method, weights = weights)
  }

}

