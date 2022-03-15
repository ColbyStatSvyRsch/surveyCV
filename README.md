# surveyCV

<!-- badges: start -->
[![R-CMD-check](https://github.com/ColbyStatSvyRsch/surveyCV/workflows/R-CMD-check/badge.svg)](https://github.com/ColbyStatSvyRsch/surveyCV/actions)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/surveyCV?color=002878)](https://cran.r-project.org/package=surveyCV)
<!-- badges: end -->


The R package `surveyCV` carries out cross-validation for complex sample survey data.  
It is a companion R package to [our SDSS 2021 presentation](https://ww2.amstat.org/meetings/sdss/2021/onlineprogram/AbstractDetails.cfm?AbstractID=309674), and to our *Stat* article ["K-fold cross-validation for complex sample surveys"](https://doi.org/10.1002/sta4.454) (published online Jan 12, 2022).

`surveyCV` is designed to work with the [`survey`](https://cran.r-project.org/package=survey) package to specify the sampling design
(strata, clusters, sampling weights, etc.),
and to account for this design when forming CV folds and estimating the CV test error.

The package currently handles the entire CV process for linear and logistic regression models. For other models, users can generate design-based CV folds with the `folds.svy()` function, then use these folds in their own custom training/testing CV loop.


## Installation

Install a stable version of the package from [CRAN](https://cran.r-project.org/package=surveyCV):

```r
install.packages("surveyCV")
```

Or, for the latest development version, install directly from GitHub:

```r
# install.packages("remotes")
remotes::install_github("ColbyStatSvyRsch/surveyCV", build_vignettes = TRUE)
```


## Usage: automated survey CV for linear or logistic regression

The function `cv.svy()` carries out K-fold CV on a dataset for a set of linear or logistic regression formulas, using specified strata, clusters, weights, and FPCs. For each model under consideration, it reports the design-based mean CV loss and a design-based estimate of its SE.  
Use `nest = TRUE` only if cluster IDs are nested within strata (i.e., if clusters in different strata might reuse the same names).

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
#>           mean     SE
#> .Model_1 22616 756.02
#> .Model_2 22536 748.01
#> .Model_3 22559 766.89

# The 2nd model (spline with 3 df) had lowest survey CV MSE,
# although it's well within one SE of the other models.
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
#>           mean     SE
#> .Model_1 22576 744.59
#> .Model_2 22436 739.81
#> .Model_3 22577 752.62

NSFG.svyglm <- svyglm(income ~ ns(age, df = 3), design = NSFG.svydes)
cv.svyglm(glm_object = NSFG.svyglm, nfolds = 4)
#>           mean     SE
#> .Model_1 22411 741.93
```


## Usage: survey CV folds for other models

The function `folds.svy()` generates design-based fold IDs for K-fold CV, using any specified strata and clusters.  
(Briefly: For a stratified sample, each fold will contain data from each stratum. For a cluster sample, a given cluster's rows will all be assigned to the same fold. See [our *Stat* paper](https://doi.org/10.1002/sta4.454) for details.)

Using these fold IDs, you can write your own CV loop for models that our packages does not currently handle.

Here is an example of tuning the bin size for a design-based random forest, using the `rpms_forest()` function from the [`rpms`](https://cran.r-project.org/package=rpms) package. Note that while `folds.svy()` accounts for the clustering, we also need to pass the cluster IDs and survey weights to `rpms_forest()` for design-consistent model-fitting, and we need to use the survey weights in the MSE calculations.

```r
library(rpms)
data(CE)

# Generate fold IDs that account for clustering in the survey design
# for a subset of the CE dataset
nfolds <- 5
CEsubset <- CE[which(CE$IRAX > 0), ]
CEsubset$.foldID <- folds.svy(CEsubset, nfolds = nfolds, clusterID = "CID")

# Use CV to tune the bin_size parameter of rpms_forest()
bin_sizes <- c(10, 20, 50, 100, 250, 500)
SSEs <- rep(0, length(bin_sizes))
for(ff in 1:nfolds) {
  train <- subset(CEsubset, .foldID != ff)
  test  <- subset(CEsubset, .foldID == ff)
  for(bb in 1:length(bin_sizes)) {
    rf <- rpms_forest(IRAX ~ EDUCA + AGE + BLS_URBN, 
                      data = train,
                      weights = ~FINLWT21, clusters = ~CID,
                      bin_size = bin_sizes[bb], f_size = 50)
    yhat <- predict(rf, newdata = test)
    res2 <- (yhat - test$IRAX)^2
    # Sum up weighted SSEs, not MSEs yet,
    # b/c cluster sizes may differ across folds and b/c of survey weights
    SSEs[bb] <- SSEs[bb] + sum(res2 * test$FINLWT21)
  }
}
# Divide entire weighted sum by the sum of weights
MSEs <- SSEs / sum(CEsubset$FINLWT21)
# Show results
cbind(bin_sizes, MSEs)
#>      bin_sizes         MSEs
#> [1,]        10 204246617270
#> [2,]        20 202870633392
#> [3,]        50 201393921358
#> [4,]       100 201085838446
#> [5,]       250 201825549231
#> [6,]       500 204155844501

# Bin size 100 had the lowest survey-weighted CV MSE estimate,
# though sizes 50 and 250 were quite similar.
# Now we could fit a random forest with bin size 100 on full `CEsubset` dataset.
```

## *Stat* paper

Our GitHub repo includes R code to reproduce figures for our *Stat* article ["K-fold cross-validation for complex sample surveys"](https://doi.org/10.1002/sta4.454) (published online Jan 12, 2022).

Scripts for the PPI and NSFG examples are in the `data-raw` folder, in the `PPI_Zambia_plot.R` and `NSFG_plot.R` scripts. We cannot share the proprietary PPI dataset, but the preprocessed NSFG dataset is included in the package as `NSFG_data`, and instructions for preprocessing the NSFG data are in the same folder in the `NSFG_data.R` script.

Simulation code is in the `plots-for-Stat-paper` vignette.


## Authors

[Jerzy Wieczorek](https://github.com/civilstat),  
[Cole Guerin](https://github.com/cole164), and  
[Thomas McMahon](https://github.com/twmcma21).
