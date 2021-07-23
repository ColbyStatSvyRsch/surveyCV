#' Subset of the 2015-2017 National Survey of Family Growth (NSFG).
#'
#' We downloaded this data from the NSFG website and cleaned it following
#' an approach posted to RPubs by Hunter Ratliff.
#'
#' Note that SECUs = Sampling Error Computation Units are effectively
#' pseudo-PSUs, nested within (pseudo-)strata. See page 35 of the NSFG
#' 2011-2013 sample design documentation for details.
#'
#' @format A data frame with 5089 rows and 17 variables:
#' \describe{
#'   \item{strata}{stratum ID (this was SEST in the original dataset)}
#'   \item{SECU}{cluster ID -- these are nested within strata}
#'   \item{wgt}{sampling weight}
#'   \item{YrEdu}{years of education}
#'   \item{income}{income in thousands of USD}
#'   \item{CASEID}{(to be documented)}
#'   \item{LBW}{(to be documented)}
#'   \item{PreMe}{(to be documented)}
#'   \item{gotPNcare}{(to be documented)}
#'   \item{KnowPreg}{(to be documented)}
#'   \item{age}{(to be documented)}
#'   \item{race}{(to be documented)}
#'   \item{BMI}{(to be documented)}
#'   \item{PregNum}{(to be documented)}
#'   \item{eduCat}{(to be documented)}
#'   \item{GA}{(to be documented)}
#'   \item{Wanted}{(to be documented)}
#' }
#' @source \url{https://www.cdc.gov/nchs/nsfg/nsfg_2015_2017_puf.htm}
#' @source \url{https://rpubs.com/HunterRatliff1/NSFG_Wrangle}
#' @source \url{https://www.cdc.gov/nchs/data/nsfg/nsfg_2011_2013_sampledesign.pdf}
"NSFG_data"

# TODO: confirm details and document the remaining variables






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
"stratdAICds"

#' @rdname StatPaperSims
"clussrsds"

#' @rdname StatPaperSims
"clusclusds"

#' @rdname StatPaperSims
"clusdAICds"

#' @rdname StatPaperSims
"srssrsds"

#' @rdname StatPaperSims
"srsdAICds"
