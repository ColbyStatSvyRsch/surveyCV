library(surveyCV)
library(rpms)

#### From example("rpms"):

# # model mean of retirement account value for households with reported
# # retirment account values > 0 using a binary tree while accounting for
# # clusterd data and sample weights.
# s1 <- which(CE$IRAX > 0)
# r1 <- rpms(IRAX~EDUCA+AGE+BLS_URBN, data = CE[s1,], weights=~FINLWT21, clusters=~CID)



#### Let's try using surveyCV to pick a bin_size for rpms_forest;
#### the default bin_size = 5 seems quite small for a dataset with 4509 rows

#### We'll also set the forest size to f_size = 50 instead of default 500
#### just to make it run faster (1.5 minutes instead of 20 minutes)
#### though I got very similar results either way

nfolds <- 5
s1 <- which(CE$IRAX > 0)
CEsubset <- CE[s1, ]
nrow(CEsubset) # 4509

CEsubset$.foldID <- folds.svy(CEsubset, nfolds = nfolds, clusterID = "CID")
names(CEsubset)
table(CEsubset$.foldID)
table(CEsubset$.foldID, CEsubset$CID)

bin_sizes <- c(10, 20, 50, 100, 250, 500)
SSEs <- rep(0, length(bin_sizes))

system.time({
  for(ff in 1:nfolds) {
    train <- subset(CEsubset, .foldID != ff)
    test  <- subset(CEsubset, .foldID == ff)
    for(bb in 1:length(bin_sizes)) {
      rf <- rpms_forest(IRAX ~ EDUCA + AGE + BLS_URBN, 
                        data = train, weights = ~FINLWT21, clusters = ~CID,
                        bin_size = bin_sizes[bb], f_size = 50)
      yhat <- predict(rf, newdata = test)
      res2 <- (yhat - test$IRAX)^2
      # weighted SSEs, not MSEs yet, b/c cluster sizes may differ across folds
      SSEs[bb] <- SSEs[bb] + sum(res2 * test$FINLWT21)
    }
  }
  MSEs <- SSEs / sum(CEsubset$FINLWT21)
})

# Print the results
cbind(bin_sizes, MSEs)


#### From a previous run with default f_size = 500,
#### instead of the faster f_size=50 above...
# > cbind(bin_sizes, MSEs)
# bin_sizes         MSEs
# [1,]        10 204246617270
# [2,]        20 202870633392
# [3,]        50 201393921358
# [4,]       100 201085838446
# [5,]       250 201825549231
# [6,]       500 204155844501

# Bin size 100 had the lowest survey-weighted MSE across the 5 survey CV folds,
# though sizes 50 and 250 were quite similar;
# any of these seem reasonable to use for a RF model on the full dataset

rf <- rpms_forest(IRAX ~ EDUCA + AGE + BLS_URBN, 
                  data = CEsubset, weights = ~FINLWT21, clusters = ~CID,
                  bin_size = 100)
