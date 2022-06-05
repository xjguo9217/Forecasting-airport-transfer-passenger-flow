# This script calculates the empirical distributions of connection times within each terminal node.
library(lubridate)
library(copula)
library(QRM)
library(ks)

# Function for calculating pinball scores
pinball <- function(pred, act, q) {
  I = 1*(act>pred)
  l <- (act - pred)*q*I + (pred - act)*(1-q)*(1-I)
  return(mean(l))
}

data = read.csv('~/Dropbox/APOC paper/code/New training and testing set/data/testingSet.csv',head=TRUE)
nn <- which(data$ob_int_dom == 'D') # for domestic
data <- data[nn,]
leaf_no_train = read.csv('leaf_train_no.csv',head=FALSE)
leaf_no_train <- leaf_no_train[-1,]
train_data = read.csv('~/Dropbox/APOC paper/code/New training and testing set/data/trainingSet_qr.csv',head=TRUE)
leaf_testing <- read.csv('leaf_test_no.csv',head=FALSE)
leaf_testing <- leaf_testing[-1,]
leaf_testing <- leaf_testing[nn,] # for domestic
train_data <- train_data$Delta
unique_leaf_train <- unique(leaf_no_train[,2])

# Calculate empirical distributions
f <- c()
bwvec <- rep(NA, length(unique_leaf_train))
for (i in 1:length(unique_leaf_train)) {
  delta_this_leaf <- train_data[which(leaf_no_train[,2] == unique_leaf_train[i])]
  ecdf <- kde(delta_this_leaf,binned=FALSE)
  f <- c(f,list(ecdf))
  bwvec[i] <- ecdf$h
}

# Calculate 5 quantiles
pred_05 <- rep(NA, nrow(data))
pred_25 <- rep(NA, nrow(data))
pred_50 <- rep(NA, nrow(data))
pred_75 <- rep(NA, nrow(data))
pred_95 <- rep(NA, nrow(data))
for (i in 1:length(unique_leaf_train)) {
  test_this_leaf <- which(leaf_testing[,2] == unique_leaf_train[i])
  pred_05[test_this_leaf] <- qkde(0.05, fhat = f[[i]])
  pred_25[test_this_leaf] <- qkde(0.25, fhat = f[[i]])
  pred_50[test_this_leaf] <- qkde(0.50, fhat = f[[i]])
  pred_75[test_this_leaf] <- qkde(0.75, fhat = f[[i]])
  pred_95[test_this_leaf] <- qkde(0.95, fhat = f[[i]])
}

# Calculate pinball scores
pinball(pred_05, data$Delta,0.05)
pinball(pred_25, data$Delta,0.25)
pinball(pred_50, data$Delta,0.50)
pinball(pred_75, data$Delta,0.75)
pinball(pred_95, data$Delta,0.95)
mean(c(pinball(pred_05, data$Delta,0.05),
       pinball(pred_25, data$Delta,0.25),
       pinball(pred_50, data$Delta,0.50),
       pinball(pred_75, data$Delta,0.75),
       pinball(pred_95, data$Delta,0.95)))

# Calculate rmse and mape
library(Metrics)
rmse(pred_50, data$Delta)
mape(pred_50, data$Delta)

# Calculate CRPS and log score
library(scoringRules)
actual <- data$Delta
log_rt <- rep(NA, length(actual))
crps_rt <- rep(NA, length(actual))
for (i in 1:length(unique_leaf_test)) {
  nn <- which(leaf_testing[,2] == unique_leaf_test[i])
  delta_this_leaf <- train_data[which(leaf_no_train[,2] == unique_leaf_test[i])]
  f1 <- f[[which(unique_leaf_train == unique_leaf_test[i])]]
  for (j in 1:length(nn)) {
    crps_rt[nn[j]] <- crps_sample(actual[nn[j]], delta_this_leaf, method = "kde", bw = bwvec[which(unique_leaf_train == unique_leaf_test[i])])
    num <- dkde(actual[nn[j]],f1)
    if (num < 0) {
      log_rt[nn[j]] <-logs_sample(actual[nn[j]], delta_this_leaf, bw = bwvec[which(unique_leaf_train == unique_leaf_test[i])])
    } else {
      log_rt[nn[j]] <- -log(num)
    }
    
  }
}
result <- cbind(crps_rt, log_rt)