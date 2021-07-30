# surveyCV

The R package `surveyCV` carries out cross-validation for complex sample survey data.  
It is a companion R package to [our SDSS 2021 presentation](https://ww2.amstat.org/meetings/sdss/2021/onlineprogram/AbstractDetails.cfm?AbstractID=309674), presented by Jerzy Wieczorek.

`surveyCV` is designed to work with the [`survey`](https://cran.r-project.org/web/packages/survey/index.html) package to specify the sampling design
(strata, clusters, sampling weights, etc.),
and to account for this design when forming CV folds and estimating the CV test error.

The package currently works for linear and logistic regression models, but we plan to make it much more general.

## Installation

Install the latest development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("civilstat/RankingProject",
                         build_vignettes = TRUE)
```

## Usage

The function `cv.svy()` carries out K-fold CV on a dataset for a set of linear or logistic regression formulas, using specified strata, clusters, weights, and FPCs.

```r
data("Auto", package = "ISLR")
cv.svy(Auto, c("mpg~poly(horsepower,1, raw = TRUE)",
               "mpg~poly(horsepower,2, raw = TRUE)"),
       nfolds = 10, strataID = "year")
```

The function `cv.svydesign()` only needs a `svydesign` object and a formula, and will parse the relevant survey design information before passing it to `cv.svy()`. Similarly, the function `cv.svyglm()` only needs a `svyglm` object, and will parse both the formula and the survey design.

## *Stat* paper

We include R code to reproduce figures for our *Stat* submission.

Scripts for the PPI and NSFG examples are in the `data-raw` folder, in the `PPI_Zambia_plot.R` and `NSFG_plot.R` scripts. We cannot share the proprietary PPI dataset, but the preprocessed NSFG dataset is included in the package as `NSFG_data`, and instructions for preprocessing the NSFG data are in the same folder in the `NSFG_data.R` script.

Simulation code is in the `plots-for-Stat-paper` vignette.

## Authors

[Jerzy Wieczorek](https://github.com/civilstat),  
[Cole Guerin](https://github.com/cole164), and  
[Thomas McMahon](https://github.com/twmcma21).
