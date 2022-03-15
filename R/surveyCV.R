#' surveyCV: Cross Validation Based on Survey Design
#'
#' Functions to generate K-fold cross validation (CV) folds
#' and CV test error estimates that take into account
#' how a survey dataset's sampling design was constructed
#' (SRS, clustering, stratification, and/or unequal sampling weights).
#' You can input linear and logistic regression models, along with data and a
#' type of survey design in order to get an output that can help you determine
#' which model best fits the data using K-fold cross validation.
#' Our paper on "K-Fold Cross-Validation for Complex Sample Surveys"
#' by Wieczorek, Guerin, and McMahon (2022)
#' <\doi{10.1002/sta4.454}>
#' explains why differing how we take folds based on survey design is useful.
#'
#' The code for this package seeks to create an alternative for the
#' \code{\link[boot:cv.glm]{boot::cv.glm}}
#' function, so that results correctly account for survey designs during
#' K-fold cross validation.

#' @importFrom survey svydesign svyglm svymean make.formula
#' @importFrom stats predict as.formula formula quasibinomial
#'
#' @docType package
#' @name surveyCV
NULL
