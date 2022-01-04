#' Subset of the 2015-2017 National Survey of Family Growth (NSFG): one birth per respondent.
#'
#' We downloaded this data from the NSFG website and cleaned it following
#' an approach posted to RPubs by Hunter Ratliff.

#'
#' Note that these data were filtered down to include only:
#'
#' - live births,
#' - with gestational ages below 45 weeks,
#' - born to mothers who were aged 20-40 years old at time of conception;
#'
#' ...then filtered further down to only the *first* such birth per respondent.
#'
#' Also note that SECUs = Sampling Error Computation Units are effectively
#' pseudo-PSUs, nested within (pseudo-)strata. See page 35 of the NSFG
#' 2011-2013 sample design documentation for details.
#'
#' @format A data frame with 2801 rows and 17 variables:
#' \describe{
#'   \item{CASEID}{Respondent ID number (per respondent, not per pregnancy)}
#'   \item{LBW}{(originally LBW1) Low birthweight (TRUE/FALSE) for the 1st baby from this pregnancy}
#'   \item{PreMe}{(recode of WKSGEST) Whether gestational age was premature (below 37 weeks) or full term}
#'   \item{gotPNcare}{(recode of BGNPRENA) Whether or not respondent got prenatal care in first trimester (before 13 weeks)}
#'   \item{KnowPreg}{(recode of KNEWPREG) Whether or not respondent learned she was pregnant by 6 weeks}
#'   \item{age}{(originally AGECON) Age at time of conception}
#'   \item{income}{(originally POVERTY) Income as percent of poverty level, so that 100 = income is at the poverty line; topcoded at 500}
#'   \item{YrEdu}{(originally EDUCAT) Education (number of years of schooling)}
#'   \item{race}{(originally HISPRACE) Race & Hispanic origin of respondent}
#'   \item{BMI}{Body Mass Index}
#'   \item{PregNum}{(originally PREGNUM) Respondent's total number of pregnancies}
#'   \item{eduCat}{(originally HIEDUC) Highest completed year of school or highest degree received}
#'   \item{GA}{(originally WKSGEST) Gestational length of completed pregnancy (in weeks)}
#'   \item{Wanted}{(recode of NEWWANTR) Whether or not pregnancy came at right time according to respondent (rather than too soon, too late, or unwanted)}
#'   \item{wgt}{(originally WGT2015_2017) Final weight for the 2015-2017 NSFG (at the respondent level, not pregnancy level)}
#'   \item{SECU}{Randomized version of cluster ID, or "sampling error computational unit" -- these are nested within strata}
#'   \item{strata}{(originally SEST) Randomized version of stratum ID}
#' }
#' @source \url{https://www.cdc.gov/nchs/nsfg/nsfg_2015_2017_puf.htm}
#' @source \url{https://rpubs.com/HunterRatliff1/NSFG_Wrangle}
#' @source \url{https://www.cdc.gov/nchs/data/nsfg/nsfg_2011_2013_sampledesign.pdf}
"NSFG_data"



#' Subset of the 2015-2017 National Survey of Family Growth (NSFG): all live births per respondent.
#'
#' Same as `NSFG_data` but using *every* birth, not just the *first* birth,
#' out of the initial subset there
#' (live births with gestational age < 45 weeks
#'  for mothers aged 20 to 40 at time of conception).
#'
#' @format A data frame with 5089 rows and 17 variables
"NSFG_data_everypreg"



#' Internal datasets for plots-for-Stat-paper vignette.
#'
#' See plots-for-Stat-paper vignette for details.
#'
#' @format Data frames containing simulations from plots-for-Stat-paper.
#' @name StatPaperSims
NULL

#' @rdname StatPaperSims
"AllW"

#' @rdname StatPaperSims
"ModW"

#' @rdname StatPaperSims
"MSEW"

#' @rdname StatPaperSims
"NoW"

#' @rdname StatPaperSims
"stratsrsds"

#' @rdname StatPaperSims
"stratstratds"

#' @rdname StatPaperSims
"stratpopds"

#' @rdname StatPaperSims
"clussrsds"

#' @rdname StatPaperSims
"clusclusds"

#' @rdname StatPaperSims
"cluspopds"

#' @rdname StatPaperSims
"srssrsds"

#' @rdname StatPaperSims
"srspopds"
