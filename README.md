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
# install.packages("remotes")
remotes::install_github("ColbyStatSvyRsch/surveyCV")
```

## Usage

The function `cv.svy()` carries out K-fold CV on a dataset for a set of linear or logistic regression formulas, using specified strata, clusters, weights, and FPCs. Use `nest = TRUE` if cluster IDs are nested within strata.

```r
library(surveyCV)
library(splines)
data(NSFG_data)
cv.svy(NSFG_data, c("income ~ ns(age, df = 2)",
                    "income ~ ns(age, df = 3)",
                    "income ~ ns(age, df = 4)"),
       nfolds = 4,
       strataID = "strata", clusterID = "SECU",
       nest = TRUE, weightsID = "wgt")
##           mean     SE
## .Model_1 22616 756.02
## .Model_2 22536 748.01
## .Model_3 22559 766.89
```

For convenience, the function `cv.svydesign()` only needs a `svydesign` object and a formula, and will parse the relevant survey design information before passing it to `cv.svy()`.  
Similarly, the function `cv.svyglm()` only needs a `svyglm` object, and will parse both the formula and the survey design.

```r
NSFG.svydes <- svydesign(id = ~SECU, strata = ~strata, nest = TRUE,
                         weights = ~wgt, data = NSFG_data)
cv.svydesign(formulae = c("income ~ ns(age, df = 2)",
                          "income ~ ns(age, df = 3)",
                          "income ~ ns(age, df = 4)"),
             design_object = NSFG.svydes, nfolds = 4)
##           mean     SE
## .Model_1 22576 744.59
## .Model_2 22436 739.81
## .Model_3 22577 752.62

NSFG.svyglm <- svyglm(income ~ ns(age, df = 3), design = NSFG.svydes)
cv.svyglm(glm_object = NSFG.svyglm, nfolds = 4)
##           mean     SE
## .Model_1 22411 741.93
```

## *Stat* paper

We include R code to reproduce figures for our *Stat* submission.

Scripts for the PPI and NSFG examples are in the `data-raw` folder, in the `PPI_Zambia_plot.R` and `NSFG_plot.R` scripts. We cannot share the proprietary PPI dataset, but the preprocessed NSFG dataset is included in the package as `NSFG_data`, and instructions for preprocessing the NSFG data are in the same folder in the `NSFG_data.R` script.

Simulation code is in the `plots-for-Stat-paper` vignette.

## Authors

[Jerzy Wieczorek](https://github.com/civilstat),  
[Cole Guerin](https://github.com/cole164), and  
[Thomas McMahon](https://github.com/twmcma21).
