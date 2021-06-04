# surveyCV

Draft of an R package to carry out cross-validation for complex sample survey data.  
Companion R package to [work presented at SDSS 2021](https://ww2.amstat.org/meetings/sdss/2021/onlineprogram/AbstractDetails.cfm?AbstractID=309674) by Jerzy Wieczorek.

This `surveyCV` package will work with the [`survey`](https://cran.r-project.org/web/packages/survey/index.html) package to specify the sampling design
(strata, clusters, sampling weights, etc.),
and to account for this design when forming CV folds and estimating the CV test error.

Authors: [Jerzy Wieczorek](https://github.com/civilstat), [Cole Guerin](https://github.com/cole164), and [Thomas McMahon](https://github.com/twmcma21).
