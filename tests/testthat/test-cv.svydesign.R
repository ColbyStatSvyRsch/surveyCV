suppressPackageStartupMessages({
  library(survey)
})

data('api', package = 'survey')

srs_design <- svydesign(data = apisrs,
                        id = ~1,
                        fpc = ~fpc)
stratified_design <- svydesign(data = apistrat,
                               id = ~1,
                               strata = ~stype,
                               weights = ~pw,
                               fpc = ~fpc)
single_stage_design <- svydesign(data = apiclus1,
                                 id = ~dnum, fpc = ~fpc,
                                 weights = ~pw)
multistage_design <- svydesign(data = apiclus2,
                               id  = ~dnum + snum,
                               fpc = ~fpc1 + fpc2)
clustered_stratified_design <- svydesign(data = apiclus1,
                                         id = ~dnum,
                                         strata = ~ stype,
                                         weights = ~ pw,
                                         nest = TRUE)

test_that("`cv.svydesign` matches `cv.svy`", {
  # SRS sample
  set.seed(2022)
  apisrs[['PSU']] <- 1:nrow(apisrs)
  apisrs[['STRATUM']] <- rep(1, nrow(apisrs))
  cv.svy_result <- cv.svy(
    Data = apisrs,
    clusterID = "PSU",
    strataID = "STRATUM",
    fpcID = 'fpc',
    nfolds = 5,
    formulae = c("api00~ell",
                 "api00~ell+meals",
                 "api00~ell+meals+mobility"),
    method = "linear"
  )
  set.seed(2022)
  cv.svydesign_result <- cv.svydesign(
    design_object = srs_design,
    nfolds = 5,
    formulae = c("api00~ell",
                 "api00~ell+meals",
                 "api00~ell+meals+mobility"),
    method = "linear"
  )
  expect_equal(
    object = cv.svydesign_result,
    expected = cv.svy_result,
    label = "SRS design"
  )

  # Single-stage stratified sample
  set.seed(2022)
  apistrat[['PSU']] <- 1:nrow(apistrat)
  cv.svy_result <- cv.svy(
    Data = apistrat,
    weightsID = "pw",
    clusterID = "PSU",
    strataID = "stype",
    fpcID = 'fpc',
    nfolds = 5,
    formulae = c("api00~ell",
                 "api00~ell+meals",
                 "api00~ell+meals+mobility"),
    method = "linear"
  )
  set.seed(2022)
  cv.svydesign_result <- cv.svydesign(
    design_object = stratified_design,
    nfolds = 5,
    formulae = c("api00~ell",
                 "api00~ell+meals",
                 "api00~ell+meals+mobility"),
    method = "linear"
  )
  expect_equal(
    object = cv.svydesign_result,
    expected = cv.svy_result,
    label = "Single-stage stratified design"
  )

  # Single-stage unstratified cluster sample
  set.seed(2022)
  apiclus1[['STRATUM']] <- rep(1, nrow(apiclus1))
  cv.svy_result <- cv.svy(
    Data = apiclus1,
    weightsID = "pw",
    clusterID = "dnum",
    strataID = "STRATUM",
    fpcID = 'fpc',
    nfolds = 5,
    formulae = c("api00~ell",
                 "api00~ell+meals",
                 "api00~ell+meals+mobility"),
    method = "linear"
  )
  set.seed(2022)
  cv.svydesign_result <- cv.svydesign(
    design_object = single_stage_design,
    nfolds = 5,
    formulae = c("api00~ell",
                 "api00~ell+meals",
                 "api00~ell+meals+mobility"),
    method = "linear"
  )
  expect_equal(
    object = cv.svydesign_result,
    expected = cv.svy_result,
    label = "Unstratified cluster design"
  )

  # Multistage unstratified cluster sample
  set.seed(2022)
  apiclus2[['STRATUM']] <- rep(1, nrow(apiclus2))
  cv.svy_result <- cv.svy(
    Data = apiclus2,
    weightsID = "pw",
    clusterID = "dnum",
    strataID = "STRATUM",
    fpc = "fpc1",
    nfolds = 5,
    formulae = c("api00~ell",
                 "api00~ell+meals",
                 "api00~ell+meals+mobility"),
    method = "linear"
  )
  suppressWarnings({
    set.seed(2022)
    cv.svydesign_result <- cv.svydesign(
      design_object = multistage_design,
      nfolds = 5,
      formulae = c("api00~ell",
                   "api00~ell+meals",
                   "api00~ell+meals+mobility"),
      method = "linear"
    )
  })
  expect_equal(
    object = cv.svydesign_result,
    expected = cv.svy_result,
    label = "Multistage, unstratified cluster design"
  )

  # Single-stage, stratified cluster sample
  apiclus1[['DNUM_BY_STYPE']] <- interaction(apiclus1$stype,
                                             apiclus1$dnum,
                                             sep = "|")
  set.seed(2022)
  cv.svy_result <- cv.svy(
    Data = apiclus1,
    weightsID = "pw",
    clusterID = "DNUM_BY_STYPE",
    strataID = "stype", nest = FALSE,
    nfolds = 5,
    formulae = c("api00~ell",
                 "api00~ell+meals",
                 "api00~ell+meals+mobility"),
    method = "linear"
  )
  set.seed(2022)
  cv.svydesign_result <- cv.svydesign(
    design_object = clustered_stratified_design,
    nfolds = 5,
    formulae = c("api00~ell",
                 "api00~ell+meals",
                 "api00~ell+meals+mobility"),
    method = "linear"
  )
  expect_equal(
    object = cv.svy_result,
    expected = cv.svydesign_result,
    label = "Single-stage, stratified cluster design"
  )

})

test_that("Informative warning for multistage designs", {
  expect_warning(
    object = {cv.svydesign(
      design_object = multistage_design,
      nfolds = 5,
      formulae = c("api00~ell",
                   "api00~ell+meals",
                   "api00~ell+meals+mobility"),
      method = "linear"
    )
      },
    regexp = "Only first-stage clusters and strata will be used"
  )
})

test_that("Informative error for replicate designs", {
  replicate_design <- single_stage_design |>
    as.svrepdesign(type = "JK1")
  expect_error(
    object = {cv.svydesign(
      design_object = replicate_design,
      nfolds = 5,
      formulae = c("api00~ell",
                   "api00~ell+meals",
                   "api00~ell+meals+mobility"),
      method = "linear"
    )
    },
    regexp = "Replicate designs are not currently supported"
  )
})

test_that("Informative error for non-design objects", {
  expect_error(
    object = {folds.svydesign(
      design_object = apiclus1,
      nfolds = 5
    )
    },
    regexp = "must be a survey design object"
  )
})
