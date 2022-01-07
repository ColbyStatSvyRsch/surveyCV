## Resubmission

This is a resubmission. In this version I have:

* Used only undirected quotation marks in the DESCRIPTION text.

* Added \value to .Rd files to describe function results and output structure,
as well as \seealso and other links to related documentation,
for each of man/cv.svy.Rd, man/cv.svydesign.Rd, and man/cv.svyglm.Rd.

* Replaced print() information messages to the console with stopifnot(),
in R/cv.svy.R.

* Removed R/plot_generation.R and vignettes/early-results.Rmd,
in which we used to set a random seed inside a function.
Also removed the associated saved simulated data from R/sysdata.rda,
and removed 'sampling' package from Suggests.
We had been using them internally to document earlier work in progress,
but this function and vignette are not needed by package users.

* Changed Depends to earlier R 4.0 (as well as Suggests for grid and splines).


## Test environments

* local Windows 10 install, R 4.1.2
* devtools::check_rhub()
* devtools::check_win_release()
* devtools::check_win_devel()
* GitHub Actions for windows-latest (release), macOS-latest (release), ubuntu-latest (release), ubuntu-latest (devel)


## R CMD check results

There were no ERRORs or WARNINGs.

There were two NOTEs:

* New submission

This is indeed a new submission. 

* Possibly misspelled words in DESCRIPTION:
  GitHub (17:62)
  SRS (12:6)
  repo (17:69)

Those words are not misspelled. SRS = simple random sample.


## Downstream dependencies

There are currently no downstream dependencies for this package.
