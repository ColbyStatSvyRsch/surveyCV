# Utility functions for creating folds based on the survey design.


# TODO: DOCUMENTATION WILL NEED TO BE UPDATED
#   (eventually, though perhaps no rush, since
#   this is not currently meant to be a user-facing function)

# TODO: Combine all these 4 cases (SRS, Clus, Strat, and Clus+Strat)
#   into one Clus+Strat case where we just
#   set all cluster IDs to 1:n if there's no clustering, and/or
#   set all stratum IDs to 1 if there's no stratification

# TODO: Change the stopifnot() error message for strata:
#   if some strata are too small and you don't want to decrease nfolds,
#   you could instead manually create larger pseudo-strata
#   by combining several smaller ones... e.g. see Lumley p43?

# TODO: Add a `verbose` argument, and if(verbose),
#   print() or cat() a report about the nr of samples and PSUs
#   in each fold or each strat-by-fold combo

# TODO: Maybe separate this out into appendfolds() vs makefolds(),
#   where appendfolds() is a wrapper (returns the dataset with .foldID in it),
#   but makefolds() returns ONLY .foldID, in case that's ever needed alone?
#   Perhaps may be useful for speeding up work with very large datasets?




# appendfolds()
# Inputs:
#   the dataset,
#   number of folds,
#   and strings for the names of the stratum and cluster variables in Data
#   (we don't ask whether nest=TRUE or FALSE because the code is written
#    to handle clusters separately within each stratum, if strata are given)
# Outputs:
#   same dataset with fold IDs appended in a .foldID variable
appendfolds <- function(Data, nfolds, strataID = NULL, clusterID = NULL) {
  stopifnot(nfolds <= nrow(Data))

  if(is.null(strataID) & is.null(clusterID)) {

    # SRS CV
    .foldID <- sample(rep(1:nfolds, length.out = nrow(Data)))

  } else if(is.null(strataID)) {

    # Clustered CV
    # Ignoring unequal cluster sizes for now,
    # just assign each cluster to a fold at random
    clus <- Data[[clusterID]]
    nClus <- length(unique(clus))
    stopifnot(nfolds <= nClus)
    foldID.clus <- sample(rep(1:nfolds, length.out = nClus))
    # Then assign these cluster-level foldIDs to the corresponding rows of data
    foldID.df <- merge(data.frame(id = 1:length(clus), clus = clus),
                       data.frame(clus = unique(clus), foldID = foldID.clus),
                       sort = FALSE)
    # Sort them to be in the dataset's original order
    foldID.df <- foldID.df[order(foldID.df$id), ]
    .foldID <- foldID.df$foldID

  } else if(is.null(clusterID)) {

    # Stratified CV
    strat <- Data[[strataID]]
    n <- nrow(Data)
    # Each fold should be able to have at least one obs from each stratum
    stopifnot(nfolds <= min(table(strat)))

    # Scramble, then reorder by stratum
    foldID.df <- data.frame(id = 1:n, strat = strat)
    foldID.df <- foldID.df[sample(1:n), ]
    foldID.df <- foldID.df[order(foldID.df$strat), ]
    # Assign fold IDs sequentially -- now folds will be balanced across strata,
    # but random within each stratum
    foldID.df$foldID <- rep(1:nfolds, length.out = n)
    # Sort them to be in the dataset's original order
    foldID.df <- foldID.df[order(foldID.df$id), ]
    .foldID <- foldID.df$foldID

  } else {

    # CV with both strata and clusters

    # TODO: (Could we make this function even more modular?
    # Make separate appendfolds functions for strat vs clus vs SRS,
    # and then just have the strat function
    # call either the clus or SRS function with each stratum?
    # ...
    # Hmmm -- perhaps not -- at least the simple/naive approach would lead to
    # having more cases in fold 1 than in fold K in *each* stratum
    # when strata sizes don't divide nfolds equally,
    # and that becomes even worse when we combine strata.)
    # So...
    # if we *don't* re-use the same function independently in each stratum,
    # can we do better at balancing folds vs strata?
    # ...
    # Sort by strata, then shuffle clusters within each stratum,
    # then assign folds sequentially to each strat.clus combo.
    # Then merge those folds back in with row IDs, and finally sort on row ID.
    # (This should work whether or not nest=TRUE,
    # i.e. whether or not clusters in different strata have different names.)

    strat <- Data[[strataID]]
    clus <- Data[[clusterID]]
    n <- nrow(Data)

    # Get all unique stratum-cluster combos
    foldID.clus <- unique(data.frame(strat, clus))
    # Each fold should be able to have at least one cluster from each stratum
    stopifnot(nfolds <= table(foldID.clus$strat))

    # Then do as for stratified CV, but at the cluster level first:
    # Scramble, then reorder by stratum
    foldID.clus <- foldID.clus[sample(1:nrow(foldID.clus)), ]
    foldID.clus <- foldID.clus[order(foldID.clus$strat), ]
    # Assign fold IDs sequentially -- now folds will be balanced across strata,
    # but random within each stratum
    foldID.clus$foldID <- rep(1:nfolds, length.out = nrow(foldID.clus))

    # Then assign these cluster-level foldIDs to the corresponding rows of data
    foldID.df <- merge(data.frame(id = 1:n, strat = strat, clus = clus),
                       foldID.clus)

    # Sort them to be in the dataset's original order
    foldID.df <- foldID.df[order(foldID.df$id), ]
    .foldID <- foldID.df$foldID

  }

  return(cbind(Data, .foldID = .foldID))
}

