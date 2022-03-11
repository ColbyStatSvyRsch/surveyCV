# surveyCV 0.2.0

* Make `folds.svy()` and `folds.svydesign()` functions directly accessible and documented, so that users can form survey-design-based CV folds and use them with other models besides `svyglm`.

# surveyCV 0.1.2

* Allow logistic regression response variable to be a factor, not just 0/1. Add logistic regression `example()` to function documentation. Add `na.rm` arguments, passed internally to `svymean` when calculating average of CV test losses.

# surveyCV 0.1.1.9001

* Development version: update docs with link to *Stat* [<doi:10.1002/sta4.454>](https://doi.org/10.1002/sta4.454) following online publication on Jan 12, 2022.

# surveyCV 0.1.1.9000

* Development version: update docs with CRAN link and installation instruction, and with news about the paper's acceptance to *Stat* on Jan 9, 2022.

# surveyCV 0.1.1

* Initial package release, as accepted to CRAN on Jan 10, 2022.

# surveyCV 0.1.0

* Initial package pre-release. Includes old simulation results reported in `early-results` vignette based on code in `R/plot_generation.R`, to be removed before CRAN submission.
