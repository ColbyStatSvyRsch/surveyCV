## Summary of changes since last CRAN version (v0.1.1)

* Update docs with link to *Stat* paper <doi:10.1002/sta4.454> published on Jan 12, 2022.
* Allow logistic regression response variable to be a factor, not just 0/1. Add logistic regression `example()` to function documentation.
* Add `na.rm` arguments, passed internally to `svymean` when calculating average of CV test losses.
* Make `folds.svy()` and `folds.svydesign()` functions directly accessible and documented, so that users can form survey-design-based CV folds and use them with other models besides `svyglm`.
* Add examples to README and `intro` vignette illustrating how to use `folds.svy()` with design-consistent random forest models from the `rpms` package.


## Test environments

* local Windows 10 install, R 4.1.3
* devtools::check_rhub()
* devtools::check_win_release()
* devtools::check_win_devel()
* GitHub Actions for windows-latest (release), macOS-latest (release), ubuntu-latest (oldrel-1), ubuntu-latest (release), ubuntu-latest (devel)


## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.


## Downstream dependencies

There are currently no downstream dependencies for this package.
