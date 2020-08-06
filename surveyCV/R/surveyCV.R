#' surveyCV: Cross Validation Based on Survey Design
#'
#' Functions to generate mean squared error estimates using cross validation,
#' based on how a survey design is constructed (SRS, Cluster, Stratification).
#' You can input linear and logistic regrssion models, along with data and a 
#' type of survey design in order to get and output that can help you determine
#' which model best fits the data using K-fold cross validation.
#' See the Intro vignette (html) to see why differing how we take folds based 
#' on survey design is useful:
#' \code{vignette("intro", package = "surveyCV")}.
#' 
#' #' The code for this package seeks to create an alternative for the
#' \code{boot::cv.glm()}
#' function, so that results correctly account for survey designs during 
#' K-fold cross validation.

#' @importFrom survey svydesign svyglm svymean
#' @importFrom stats predict
#'
#' @docType package
#' @name surveyCV
NULL