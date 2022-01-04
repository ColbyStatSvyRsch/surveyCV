# Our `NSFG_data` dataset is a pre-processed version of the
# 2015-2017 National Survey of Family Growth (NSFG), specifically the
# Female Respondent Data File (2015_2017_FemRespData.dat) and
# Female Pregnancy Data File (2015_2017_FemPregData.dat).
# Both original datasets are available here,
# in the "Downloadable Data Files" section,
# and the SAS codebooks (with column names and factor levels for each variable)
# are in the "Program Statements" section:
# https://www.cdc.gov/nchs/nsfg/nsfg_2015_2017_puf.htm

# We pre-processed the data following Hunter Ratliff's code on RPubs here,
# up through the creation and editing of the `data0` dataset:
# https://rpubs.com/HunterRatliff1/NSFG_Wrangle

# Note that Ratliff filtered down only to:
# - live births,
# - with gestational ages below 45 weeks,
# - born to mothers who were aged 20-40 years old at time of conception.
#
# ...and then we filtered further down to only the *first* such birth per respondent.



# TODO: Remove the .csv, .sas, and .dat files from the ~/data-raw directory
#   and only keep the code here?
# Or is it OK to keep the files in data-raw along with source code on GitHub,
#   since they won't become part of package-bundle-to-be-installed anyway?



#### Our own initial data-processing code ####
#
#

library(dplyr)
library(SAScii)

# Translate SAS input instructions for .dat data files into R-readable format,
# read in the data, and save as CSVs
# (took 10-15 minutes on a vintage 2018 laptop)

resp <- read.SAScii("./data-raw/2015_2017_FemRespData.dat",
                    "./data-raw/2015_2017_FemRespSetup.sas")
readr::write_csv(resp, file = "./data-raw/respAll.csv")

preg <- read.SAScii("./data-raw/2015_2017_FemPregData.dat",
                    "./data-raw/2015_2017_FemPregSetup.sas")
readr::write_csv(preg, file = "./data-raw/pregAll.csv")



# SAScii did not seem to successfully create levels for factor variables,
# so we do that "manually" based on reading the code in the .sas files

resp <- readr::read_csv(file = "./data-raw/respAll.csv")
preg <- readr::read_csv(file = "./data-raw/pregAll.csv")

resp <- resp %>%

  select(CASEID, EDUCAT, HISPRACE, BMI, PREGNUM, HIEDUC) %>%

  mutate(
    HISPRACE = factor(HISPRACE, levels = 1:4,
                      labels = c("Hispanic", "Non-Hispanic White",
                                 "Non-Hispanic Black", "Non-Hispanic Other")),
    BMI = ifelse(BMI > 94, NA, BMI),
    HIEDUC = factor(HIEDUC, levels = 5:15,
                    labels = c("9TH GRADE OR LESS",
                               "10TH GRADE", "11TH GRADE", "12TH GRADE, NO DIPLOMA (NOR GED)",
                               "HIGH SCHOOL GRADUATE (DIPLOMA OR GED)",
                               "SOME COLLEGE BUT NO DEGREE", "ASSOCIATE DEGREE IN COLLEGE/UNIVERSITY",
                               "BACHELOR'S DEGREE", "MASTER'S DEGREE", "DOCTORATE DEGREE", "PROFESSIONAL DEGREE"))
  )

summary(resp)
table(resp$HIEDUC)

# CASEID ("Respondent ID number")
# EDUCAT -> YrEdu ("Education (number of years of schooling) (RECODE)":
#                  9 is 9th grade or less; 13 is 1 year of college/grad school; up through 19 is 7+ years of college/grad school)
# HISPRACE -> race ("Race & Hispanic origin of respondent - 1977 OMB standards (RECODE)")
# BMI ("Body Mass Index (computed in post-processing)"; numeric in 15-60; 95 is code for "could not be defined")
# PREGNUM -> PregNum ("CAPI-based total number of pregnancies (RECODE)")
# HIEDUC -> eduCat ("Highest completed year of school or highest degree received (RECODE)")

#'   \item{CASEID}{Respondent ID number (per respondent, not per pregnancy)}
#'   \item{YrEdu}{(originally EDUCAT) Education (number of years of schooling)}
#'   \item{eduCat}{(originally HIEDUC) Highest completed year of school or highest degree received}
#'   \item{race}{(originally HISPRACE) Race & Hispanic origin of respondent}
#'   \item{BMI}{Body Mass Index}
#'   \item{PregNum}{(originally PREGNUM) Respondent's total number of pregnancies}




preg <- preg %>%

  select(CASEID, KNEWPREG, BGNPRENA, LBW1,
         OUTCOME, AGECON, POVERTY, WKSGEST,
         NEWWANTR,
         WGT2015_2017, SECU, SEST) %>%

  mutate(
    KNEWPREG = ifelse(KNEWPREG > 97, NA, KNEWPREG),
    BGNPRENA = ifelse(BGNPRENA > 97, NA, BGNPRENA),
    LBW1 = factor(LBW1, levels = 1:2,
                  labels = c("YES, LOW BIRTH WEIGHT", "NO, NOT LOW BIRTH WEIGHT")),
    OUTCOME = factor(OUTCOME, levels = 1:6,
                     labels = c("LIVE BIRTH", "INDUCED ABORTION", "STILLBIRTH",
                                "MISCARRIAGE", "ECTOPIC PREGNANCY", "CURRENT PREGNANCY")),
    WKSGEST = ifelse(WKSGEST > 97, NA, WKSGEST),
    NEWWANTR = factor(NEWWANTR, levels = 1:7,
                      labels = c("Later, overdue", "Right time",
                                 "Too soon: by less than 2 years",
                                 "Too soon: by 2 years or more",
                                 "Didn't care, indifferent", "Unwanted",
                                 "Don't know, not sure"))

  )

summary(preg)
table(preg$WKSGEST) # strange to have one each at 49 and 73 weeks, but apparently not NAs


# CASEID ("Case identification number")
# KNEWPREG -> KnowPreg ("Weeks pregnant when R learned she was pregnant"; 98-99 are NA)
# BGNPRENA -> gotPNcare ("Weeks pregnant at first prenatal care visit"; *assuming* 99=NA?)
# LBW1 -> LBW ("Low birthweight - 1st baby from this preg")
#
# OUTCOME needed for filtering ("Pregnancy outcome")
# AGECON -> age ("Age at time of conception")
# POVERTY -> income ("Poverty level income", ie income as percent of poverty level, eg 100 = at the poverty line; topcoded at 500)
# WKSGEST -> GA ("Gestational length of completed pregnancy (in weeks)"; 98-99 = NA)
#
# NEWWANTR -> Wantedness ("Detailed wantedness of pregnancy - respondent")
# WGT2015_2017 -> wgt ("Final weight for the 2015-2017 NSFG")
# SECU ("Randomized version of the sampling error computational unit")
# SEST -> strata ("Randomized version of the stratum")

#'   \item{KnowPreg}{(recode of KNEWPREG) Whether or not respondent learned she was pregnant by 6 weeks}
#'   \item{LBW}{(originally LBW1) Low birthweight (TRUE/FALSE) for the 1st baby from this pregnancy}
#'   \item{age}{(originally AGECON) Age at time of conception}
#'   \item{income}{(originally POVERTY) Income as percent of poverty level, so that 100 = income is at the poverty line; topcoded at 500}
#'   \item{GA}{(originally WKSGEST) Gestational length of completed pregnancy (in weeks)}
#'
#'   \item{PreMe}{(recode of WKSGEST) Whether gestational age was premature (below 37 weeks) or full term}
#'   \item{gotPNcare}{(recode of BGNPRENA) Whether or not respondent got prenatal care in first trimester (before 13 weeks)}
#'   \item{Wanted}{(recode of NEWWANTR) Whether or not pregnancy came at right time according to respondent (rather than too soon, too late, or unwanted)}
#'
#'   \item{wgt}{(originally WGT2015_2017) Final weight for the 2015-2017 NSFG (at the respondent level, not pregnancy level)}
#'   \item{SECU}{Randomized version of cluster ID, or "sampling error computational unit" -- these are nested within strata}
#'   \item{strata}{(originally SEST) Randomized version of stratum ID}

readr::write_csv(resp, file = "./data-raw/resp.csv")
readr::write_csv(preg, file = "./data-raw/preg.csv")

#
# End of our initial data-processing code
####



#### Start of Hunter Ratliff's code ####
# (lightly edited to remove redundant steps and fix typos)
#

## Read & handle the female response csv

resp <- readr::read_csv("./data-raw/resp.csv") %>%
  select(CASEID, EDUCAT, HISPRACE, BMI, PREGNUM, HIEDUC) %>%

  mutate(
    # BMI's 95 and above are coded as missing
    BMI = ifelse(BMI>94, NA, BMI),

    # Recode some of the default labels from SAS
    HISPRACE = recode(HISPRACE, "Non-Hispanic Black"="Black",
                      "Non-Hispanic White"="White",
                      "Non-Hispanic Other"="Other")
  ) %>%

  # Make characters factors
  mutate_if(is.character, factor)



## Read & handle the pregnancy csv

preg <- readr::read_csv("./data-raw/preg.csv") %>%
  select(CASEID, KNEWPREG, BGNPRENA, LBW=LBW1,
         OUTCOME, AGECON, POVERTY, GA=WKSGEST,
         Wantedness=NEWWANTR,
         wgt=WGT2015_2017, SECU, strata=SEST) %>%


  mutate(
    # Anything above 94 is missing data per codebook
    GA       = ifelse(GA>94, NA, GA),
    BGNPRENA = ifelse(BGNPRENA>94, NA, BGNPRENA),
    KNEWPREG = ifelse(KNEWPREG>94, NA, KNEWPREG),

    # Recode some of the default labels from SAS
    LBW = recode(LBW,
                 "NO, NOT LOW BIRTH WEIGHT"=F,
                 "YES, LOW BIRTH WEIGHT"=T)
  ) %>%

  # A gestational age over 44 weeks isn't biologically reasonable
  filter(GA < 45) %>%

  # Make characters factors
  mutate_if(is.character, factor)



## Join data

data0 <-
  left_join(preg, resp) %>%
  # must be between 20 - 40 years old and have had a live birth
  filter(AGECON>=20, AGECON<=40, OUTCOME=="LIVE BIRTH") %>%

  # OUTCOMES
  mutate(
    # Know if pregnant by 6 weeks
    KnowPreg = factor(if_else(KNEWPREG<=6, "Yes", "No")),

    # Got prenatal care in first trimester
    gotPNcare = factor(if_else(BGNPRENA<13, "Yes", "No")),

    # Premature delivery
    PreMe = factor(if_else(GA<37, "Premature", "Term"))
  ) %>%


  # PREDICTORS
  mutate(
    Wanted   = if_else(Wantedness == "Right time", T, F),
    HIEDUC   = recode(HIEDUC,
                      "9TH GRADE OR LESS"="<HS",
                      "10TH GRADE"="<HS", "11TH GRADE"="<HS",
                      "12TH GRADE, NO DIPLOMA (NOR GED)"="<HS",
                      "HIGH SCHOOL GRADUATE (DIPLOMA OR GED)"="HS or GED",
                      "SOME COLLEGE BUT NO DEGREE"="Some college",
                      "ASSOCIATE DEGREE IN COLLEGE/UNIVERSITY"="Associates",
                      "BACHELOR'S DEGREE"="Bachelors",
                      "MASTER'S DEGREE"="Grad/prof school",
                      "PROFESSIONAL DEGREE"="Grad/prof school",
                      "DOCTORATE DEGREE"="Grad/prof school"
    )
  ) %>%

  # Make characters factors
  mutate_if(is.character, factor) %>%

  select(CASEID,
         # Outcome vars
         LBW, PreMe, gotPNcare, KnowPreg,
         age=AGECON, income=POVERTY, YrEdu=EDUCAT, race=HISPRACE, BMI, PregNum=PREGNUM,
         eduCat=HIEDUC,
         GA, Wanted,
         wgt:strata)


#
# End of Hunter Ratliff's code
####



#### Our own further data-processing code ####
#
#

NSFG_data_everypreg <- as.data.frame(data0)
readr::write_csv(NSFG_data_everypreg, file = "./data-raw/NSFG_data_everypreg.csv")
save("NSFG_data_everypreg", file = "./data/NSFG_data_everypreg.rda")

# The data above are at the PREGNANCY level --
#   each row is one pregnancy, so some mothers are in the dataset many times.
# However, the survey sampling actually happened at the RESPONDENT level.
# For purposes of illustrating the `surveyCV` package,
# it will be simpler to report an analysis at the RESPONDENT level,
# so let's subset to just the first row (first pregancy) for each CASEID:

NSFG_data <- NSFG_data_everypreg[!duplicated(NSFG_data_everypreg$CASEID), ]
readr::write_csv(NSFG_data, file = "./data-raw/NSFG_data.csv")
save("NSFG_data", file = "./data/NSFG_data.rda")

#
#
####
