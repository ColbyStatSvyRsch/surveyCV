# Utility functions for creating folds based on the survey design. 

# (After this is done:
#  Write a wrapper for it that reads strata/clusters out of svydesign object.
#  Call this function from with a CV function that *uses* these folds.)


# Inputs:
#   the dataset, 
#   number of folds, 
#   and strings for the names of the stratum and cluster variables in Data
# Outputs: 
#   same dataset with fold IDs appended
makefolds <- function(Data, nfolds, strataID = NULL, clusterID = NULL) {
  stopifnot(nfolds <= nrow(Data))
  
  if(is.null(strataID) & is.null(clusterID)) {
    
    # SRS CV
    foldID <- sample(rep(1:nfolds, length.out = nrow(Data)))
    
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
    foldID.df <- foldID.df[order(foldID.df$id), ]
    foldID <- foldID.df$foldID
    
    # if(FALSE){
    #   # CONTINUED TESTING
    #   # make sure the merged df is in the same order as before merging
    #   head(clus, 10)
    #   head(foldID.df, 10)
    #   # make sure all cases from a given cluster are in the same fold
    #   table(foldID.df$clus, foldID.df$foldID)
    # }

  } else if(is.null(clusterID)) {
    # TODO: Stratified CV
    
  } else {
    # TODO: CV with both strata and clusters
    
    # TODO: first, remake cluster names to ensure they are unique across strata
    # (i.e. how to deal with nest=TRUE?)
  }
  
  return(cbind(Data, foldID = foldID))
}