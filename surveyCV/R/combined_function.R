#' CV Survey GLM function
#'
#' This function combines the other cross validation functions in this package
#' into a single function that is able to interpret datasets and also
#' survey design objects and survey glms
#'
#' @param nfolds Number of folds to be used during cross validation, defaults to
#'   5
#' @param clusterID String of the variable used to cluster during sampling, must
#'   be the same as in the dataset used, ignore if clustering was not used
#' @param strataID String of the variable used to stratify during sampling, must
#'   be the same as in the dataset used, ignore if stratification was not used
#' @param glm Name of a Survey GLM created from the survey package if one is
#'   to be used, defaults to NULL
#' @param N Number equal to the total population size
#' @param method string, must be either linear or logistic, determines type of
#'   model fit during cross validation, defaults to linear
#' @param weights Variable in data set that contains PPS weights
#'   @examples
#' #MSEs generated for different tests of first and second degree polynomial
#' # fits predicting mpg from horsepower in the Auto Dataset. Clustering and
#' Stratification was done along the "year" variable.
#' #Using a survey glm to generate MSEs.
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
#' cv.svyglm(glm = srs.model, nfolds = 10, N = 1000)
#' cv.svyglm(glm = clus.model, nfolds = 10, N = 1000)
#' cv.svyglm(glm = strat.model, nfolds = 10, N = 1000)
#' @export



cv.svyglm <- function(nfolds=5, N, clusterID = NULL, strataID = NULL, method = "linear", weights = NULL, glm = NULL) {
  # When a glm object is specified, then the function can pull pieces of information needed from the glm.
  # This takes the formula from the model and reformats it so that it can be made into a string 
  # so it can be used in our general cv.svy() function.
  # Any reformating done is because the glm objects are weird with formulas that contain parenthases.
  # In this, `b` is what makes up the left side of our formula, and `c` makes up the right side of the formula.
  if (is.null(glm) == FALSE) {
    model = glm
    b <- paste0(model[["formula"]][[2]])
    c <- paste0(model[["formula"]][[3]])
    # `b` is a list, and if `b` has a length of 2, then the left side of the formula includes something like the log() function.
    # We need to format the left side of our formula like this.
    if (length(b) == 2) {

      b1 <- b[1]
      b2 <- b[2]
      ys <- paste0(b1, "(", b2, ")")
      # `c` is a list, and if `c` has a length of 3, then that is because the formula includes 
      # something like the natural splines function or the poly() function with  additional arguments, 
      # so we must split `c` to find out which one.
      if (length(c) == 3) {
        c1 <- c[1]
        c2 <- c[2]
        c3 <- c[3]
        # This is how we format the right side of the formula if it includes the ns() or poly() functions
        # with additional arguments.
        if (c1 == "ns") {
          xs <- paste0(c1, "(", c2, ",df=", c3, ")")
        } else if (c1 == "poly") {
          xs <- paste0(c1, "(", c2, ",raw=", c3, ")")
        } else {xs <- paste0(c2, c1, c3)}
        # `c` is a list, and if `c` has a length of 2, then that is because the formula includes 
        # something like the ns() or poly() or log() functions without additional arguments.
        # This is how we format the right side of the formula for this scenario.
      } else if (length(c) == 2) {
        c1 <- c[1]
        c2 <- c[2]
        xs <- paste0(c1, "(", c2, ")")
        # `c` is a list, and if `c` has a length of 1, then that is because the formula only includes
        # normal variables that  are not separated by parenthasis.
        # This is how we format the right side of the formula for this scenario.
      } else if (length(c) == 1) {
        xs <- c
      }
      # Formats our formula into the string it needs to be so our cv.svy() function can use it.
      formulae = paste0(ys, "~", xs)
      # `b` is a list, and if `b` has a length of 1, we have to do the same process as above.
    } else if (length(b) == 1) {
      # `c` is a list, and if `c` has a length of 3, then that is because the formula includes 
      # something like the natural splines function or the poly() function with  additional arguments, 
      # so we must split `c` to find out which one.
      if (length(c) == 3) {
        c1 <- c[1]
        c2 <- c[2]
        c3 <- c[3]
        # This is how we format the right side of the formula if it includes the ns() or poly() functions
        # with additional arguments.
        if (c1 == "ns") {
          xs <- paste0(c1, "(", c2, ",df=", c3, ")")
        } else if (c1 == "poly") {
          xs <- paste0(c1, "(", c2, ",raw=", c3, ")")
        } else {xs <- paste0(c2, c1, c3)}
        # `c` is a list, and if `c` has a length of 2, then that is because the formula includes 
        # something like the ns() or poly() or log() functions without additional arguments.
        # This is how we format the right side of the formula for this scenario.
      } else if (length(c) == 2) {
        c1 <- c[1]
        c2 <- c[2]
        xs <- paste0(c1, "(", c2, ")")
        # `c` is a list, and if `c` has a length of 1, then that is because the formula only includes
        # normal variables that  are not separated by parenthasis.
        # This is how we format the right side of the formula for this scenario.
      } else if (length(c) == 1) {
        xs <- c
      }
      # Formats our formula into the string it needs to be so our cv.svy() function can use it.
      formulae = paste0(model[["formula"]][[2]], "~", xs)

    }
    # Specifies the design object from the gilm object
    design_object = model[["survey.design"]]

  }
  # Runs our cv.svydesign() function using the pieces pulled from the glm object,
  # which will later push all of this information into our general cv.svy() function.
  cv.svydesign(formulae = formulae, nfolds = nfolds, method = method, N = N)

  }

