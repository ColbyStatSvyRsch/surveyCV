# Utility functions for creating folds based on the survey design. 

# (Next steps:
#  Write a wrapper for appendfolds() that reads strata/clusters out of svydesign object.
#  Call appendfolds() from with a separate CV function that *uses* these folds.)



# TODO: DOCUMENTATION WILL NEED TO BE UPDATED
# Inputs:
#   the dataset, 
#   number of folds, 
#   and strings for the names of the stratum and cluster variables in Data
# Outputs: 
#   same dataset with fold IDs appended
appendfolds <- function(Data, nfolds, strataID = NULL, clusterID = NULL) {
  stopifnot(nfolds <= nrow(Data))
  
  if(is.null(strataID) & is.null(clusterID)) {
    
    # SRS CV
    .foldID <- sample(rep(1:nfolds, length.out = nrow(Data)))
    
  } else if(is.null(strataID)) {
    
    # if(FALSE){
    #   # TESTING with 8 clusters, 3 folds, n=100
    #   clus <- sample(rep(1:8, length.out = 100))
    #   nfolds <- 3
    # }

    
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
    
    # if(FALSE){
    #   # CONTINUED TESTING
    #   # make sure the merged df is in the same order as before merging
    #   head(clus, 10)
    #   head(foldID.df, 10)
    #   # make sure all cases from a given cluster are in the same fold
    #   table(foldID.df$clus, foldID.df$foldID)
    # }

  } else if(is.null(clusterID)) {
    
    # if(FALSE){
    #   # TESTING with 8 strata, 3 folds, n=100
    #   strat <- sample(rep(1:8, length.out = 100))
    #   nfolds <- 3
    # }

    # Stratified CV
    strat <- Data[[strataID]]
    n <- length(strat)
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

    # if(FALSE){
    #   # CONTINUED TESTING
    #   # make sure the merged df is in the same order as before merging
    #   head(strat, 10)
    #   head(foldID.df, 10)
    #   # make sure all cases from a given stratum are equally spread across folds
    #   table(foldID.df$strat, foldID.df$foldID)
    # }
  
  } else {
    
    # TODO: CV with both strata and clusters
    # (Could we make this function even more modular?
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

    
    # if(FALSE){
    #   # TESTING with 4 strata, 8 clusters in each stratum, 3 folds, n=100
    #   strat <- rep(1:4, each = 25)
    #   clus <- rep(rep(1:8, length.out = 25), 4)
    #   nfolds <- 3
    #   data.frame(strat, clus)
    #   table(paste(strat, clus, sep = "."))
    # }
        
    strat <- Data[[strataID]]
    clus <- Data[[clusterID]]
    n <- length(strat)
    
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
    
    
    # if(FALSE){
    #   # CONTINUED TESTING
    #   # make sure the merged df is in the same order as before merging
    #   head(data.frame(strat, clus), 10)
    #   head(foldID.df, 10)
    #   # make sure all *clusters* from a given stratum are equally spread across folds
    #   addmargins(table(strat = foldID.clus$strat, fold = foldID.clus$foldID))
    #   # make sure all *cases* from a given stratum are equally spread across folds
    #   addmargins(table(strat = foldID.df$strat, fold = foldID.df$foldID))
    #   # make sure all cases from a given strat-cluster pair are in the same fold
    #   addmargins(table(strat.clus = paste(foldID.df$strat,foldID.df$clus,sep="."), fold = foldID.df$foldID))
    # }
    
  }
  
  return(cbind(Data, .foldID = .foldID))
}




# More informal testing of the function
if(FALSE) {
  x <- rnorm(100)
  strat <- rep(1:4, each = 25)
  # clus <- rep(1:8, length.out = 100)
  clus <- rep(rep(1:8, length.out = 25), 4)
  df <- data.frame(x, strat, clus)
  df <- df[sample(1:100), ]

  addmargins(table(strat = df$strat))
  addmargins(table(clus = df$clus))
  addmargins(table(strat = df$strat, clus = df$clus))
  
  
  # TEST SRS
  df.f <- appendfolds(Data = df, nfolds = 3)
  head(df)
  head(df.f)
  # Make sure all cases are about equally spread across folds
  addmargins(table(fold = df.f$.foldID))
  
  
  # TEST CLUSTERS
  df.f <- appendfolds(Data = df, nfolds = 3, clusterID = "clus")
  head(df)
  head(df.f)
  # Make sure all cases from a given cluster are in the same fold
  addmargins(table(clus = df.f$clus, fold = df.f$.foldID))
  # Make sure all *clusters* are about equally spread across folds
  addmargins(table(fold = df.f[row.names(unique(data.frame(clus))),]$.foldID))
  # Are all *cases* about equally spread across folds?
  # (Not necessarily expected if we have unequal cluster sizes)
  addmargins(table(fold = df.f$.foldID))
  
  
  # TEST STRATA
  df.f <- appendfolds(Data = df, nfolds = 3, strataID = "strat")
  head(df)
  head(df.f)
  # Make sure all *cases from a given stratum* are about equally spread across folds
  addmargins(table(df.f$strat, df.f$.foldID))
  # Make sure all cases are about equally spread across folds
  addmargins(table(fold = df.f$.foldID))
  
  
  # TEST COMBINED STRATA + CLUSTERS
  df.f <- appendfolds(Data = df, nfolds = 3, strataID = "strat", clusterID = "clus")
  head(df)
  head(df.f)
  # Make sure all *clusters* from a given stratum are about equally spread across folds
  with(df.f[row.names(unique(data.frame(strat, clus))),],
       addmargins(table(strat = strat, fold = .foldID)))
  # Make sure all *cases* from a given stratum are about equally spread across folds
  addmargins(table(strat = df.f$strat, fold = df.f$.foldID))
  # Make sure all cases from a given strat-clus *pair* are in the same fold
  # (each row here should have just one nonzero entry)
  addmargins(table(strat.clus = paste(df.f$strat, df.f$clus, sep="."), fold = df.f$.foldID))
  # Are all *cases* about equally spread across folds?
  # (Not necessarily expected if we have unequal cluster sizes)
  addmargins(table(fold = df.f$.foldID))
  
  
  # TEST STOPIFNOT -- these should all give errors
  df.f <- appendfolds(Data = df, nfolds = 10, clusterID = "clus")
  df.f <- appendfolds(Data = df, nfolds = 30, strataID = "strat")
  df.f <- appendfolds(Data = df, nfolds = 10, strataID = "strat", clusterID = "clus")
  
}
