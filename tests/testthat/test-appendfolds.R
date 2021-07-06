library(magrittr)

n <- 100
nstrat <- 4
nclus <- 8

x <- rnorm(n)
strat <- rep(1:nstrat, each = n/nstrat)
clus <- rep(rep(1:nclus, length.out = n/nstrat), nstrat)
df <- data.frame(x, strat, clus)
df <- df[sample(1:n), ]



test_that("SRS folds spread out cases evenly across folds", {
  df.f <- appendfolds(Data = df, nfolds = 3)

  # Make sure rows are still in original order
  expect_equal(row.names(df), row.names(df.f))

  # Make sure all cases are about equally spread across folds:
  # either all folds have same nr of cases, or min = max - 1
  expect_equal(table(fold = df.f$.foldID) %>%
                 range %>%
                 diff,
               0,
               tolerance = 1)
})


test_that("Cluster folds spread out cases evenly across folds", {
  df.f <- appendfolds(Data = df, nfolds = 3, clusterID = "clus")

  # Make sure rows are still in original order
  expect_equal(row.names(df), row.names(df.f))

  # Make sure all cases from a given cluster are in the same fold:
  # table() the dataset with rows=clus, cols=folds,
  # and ensure there's exactly one nonzero entry in each row
  expect_equal((table(clus = df.f$clus, fold = df.f$.foldID) > 0) %>%
                 rowSums %>%
                 unname,
               rep(1, nclus))

  # Make sure all *clusters* are about equally spread across folds:
  # either all folds have same nr of clusters, or min = max - 1
  expect_equal(table(fold = df.f[row.names(unique(data.frame(clus))),]$.foldID) %>%
                 range %>%
                 diff,
               0,
               tolerance = 1)

  # Should all *cases* be about equally spread across folds?
  # No, not necessarily expected if we have unequal cluster sizes...
  # addmargins(table(fold = df.f$.foldID))
})





test_that("Stratified folds spread out cases evenly across folds", {
  df.f <- appendfolds(Data = df, nfolds = 3, strataID = "strat")

  # Make sure rows are still in original order
  expect_equal(row.names(df), row.names(df.f))

  # Make sure all cases from a given stratum are about equally spread across folds:
  # table() the dataset with rows=strat, cols=folds,
  # and ensure the range is 0 or 1 within each row
  expect_equal(table(df.f$strat, df.f$.foldID) %>%
                 apply(1, function(x) diff(range(x))) %>%
                 unname,
               rep(0, nstrat),
               tolerance = 1)

  # Make sure all cases are about equally spread across folds:
  # either all folds have same nr of cases, or min = max - 1
  expect_equal(table(fold = df.f$.foldID) %>%
                 range %>%
                 diff,
               0,
               tolerance = 1)
})


# TODO: add tests for the combined strat+clus case
