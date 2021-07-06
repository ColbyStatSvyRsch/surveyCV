n <- 100
nstrat <- 4
nclus <- 8

x <- rnorm(n)
strat <- rep(1:nstrat, each = n/nstrat)
clus <- rep(rep(1:nclus, length.out = n/nstrat), nstrat)
df <- data.frame(x, strat, clus)
df <- df[sample(1:n), ]

test_that("SRS folds put equal nr of cases in each fold", {
  df.f <- appendfolds(Data = df, nfolds = 3)

  # Make sure all cases are about equally spread across folds:
  # either all folds have same nr of cases, or min = max - 1
  expect_equal(diff(range(table(fold = df.f$.foldID))), 0,
               tolerance = 1)
})


test_that("Cluster folds put equal nr of cases in each fold", {
  df.f <- appendfolds(Data = df, nfolds = 3, clusterID = "clus")

  # Make sure all cases from a given cluster are in the same fold:
  # table() the dataset with rows=clus, cols=folds,
  # and ensure there's exactly one nonzero entry in each row
  expect_equal(unname(rowSums(table(clus = df.f$clus, fold = df.f$.foldID) > 0)),
               rep(1, nclus))

  # Make sure all *clusters* are about equally spread across folds:
  # either all folds have same nr of clusters, or min = max - 1
  expect_equal(diff(range(table(fold = df.f[row.names(unique(data.frame(clus))),]$.foldID))), 0,
               tolerance = 1)

  # Should all *cases* be about equally spread across folds?
  # No, not necessarily expected if we have unequal cluster sizes...
  # addmargins(table(fold = df.f$.foldID))
})
