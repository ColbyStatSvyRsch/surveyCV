#' surveyCV: Cross Validation Based on Survey Design
#'
#' Functions to generate test error estimates using cross validation,
#' based on how a survey design is constructed
#' (SRS, clustering, stratification, and/or unequal sampling weights).
#' You can input linear and logistic regression models, along with data and a
#' type of survey design in order to get an output that can help you determine
#' which model best fits the data using K-fold cross validation.
#' Our draft paper on "K-Fold Cross-Validation for Complex Sample Surveys"
#' (under review; a copy is in the \code{data-raw} folder of our GitHub repo)
#' explains why differing how we take folds based on survey design is useful.
#'
#' The code for this package seeks to create an alternative for the
#' \code{boot::cv.glm()}
#' function, so that results correctly account for survey designs during
#' K-fold cross validation.

#' @importFrom survey svydesign svyglm svymean make.formula
#' @importFrom stats predict as.formula formula quasibinomial
#'
#' @docType package
#' @name surveyCV
NULL
