# This script calculates the distribution of passenger flows (# of passengers arriving in each 15 min interval) when assuming passenger arrivals are independent. Here we show the example for domestic passenger flows.
library(gtools)
library(lubridate)
data <- read.csv('~/Dropbox/APOC paper/code/New training and testing set/data/testingSet.csv',head = TRUE)
nn <- which(data$ob_int_dom == 'D') # for domestic
data <- data[nn,] # for domestic
leaf_no_train = read.csv('leaf_train_no.csv',head=FALSE)
leaf_no_train <- leaf_no_train[-1,]
train_data = read.csv('~/Dropbox/APOC paper/code/New training and testing set/data/trainingSet_qr.csv',head=TRUE)
leaf_testing <- read.csv('leaf_test_no.csv',head=FALSE)
leaf_testing <- leaf_testing[-1,]
leaf_testing <- leaf_testing[nn,]
train_data <- train_data$Delta
unique_leaf_train <- unique(leaf_no_train[,2])

on_chock = as.POSIXct(strptime(data$on_chocks_time, "%Y-%m-%d %H:%M:%S"), tz = "UTC")
on_chock_date = date(on_chock)
on_chock_date_unique = unique(on_chock_date)
n1 <- length(on_chock_date_unique)
df = rep(ymd_hms(paste(on_chock_date_unique[1], '00:00:00'), tz = "UTC"),96*n1) # There are 96 15 min within a day.

# Create a data frame of time stamps separated by 15 min.
for (i in 1:n1) {
  df[((i-1)*96+1) : (i*96)] <- seq(ymd_hms(paste(on_chock_date_unique[i], '00:00:00'), tz = "UTC"), by = '15 min',length.out=(96))
}
df = as.POSIXct(strptime(df, "%Y-%m-%d %H:%M:%S"), tz = "UTC")

# Calculate the actual passenger flow at BA's conformance desk (immigration/security areas).
confTime = strptime(data$local_conform_time, "%Y-%m-%d %H:%M:%S", tz = "UTC") # confirmation/arrival time at BA's comformance desk. 
confTime1 = with(confTime, as.POSIXct((ceiling(as.numeric(confTime)/(15*60))) * (15*60), origin = "1970-01-01", tz='UTC'))
countPax1 = rep(NA,96*length(on_chock_date_unique))
for (i in 1:length(on_chock_date_unique)) {
  this_date <- on_chock_date_unique[i]
  n <- which(on_chock_date==this_date)
  this_confTime <- confTime1[n]
  this_confTime <- c(as.POSIXct(paste(this_date,"00:00:00"), tz='UTC'), this_confTime, as.POSIXct(paste(this_date,"23:45:00"), tz='UTC'))
  countPax = data.frame(table(cut(this_confTime, breaks = "15 mins")))
  countPax$Freq[1] = countPax$Freq[1] - 1
  countPax$Freq[96] = countPax$Freq[96] - 1
  countPax1[((i-1)*96+1) : (i*96)] = countPax$Freq
}

# Calculate each passenger's probability of arriving before each of the 15 minutes interval (calculate CDF). The output "prob" is a list. Each element in this list is a vector of probabilities for a passenger in the testing set.
pp <- c()
pp1 <- c()
prob <- c()
for (i in 1:nrow(data)) {
   this_delta <- difftime(df, ymd_hms(data$on_chocks_time[i]), units="mins")
   nn <- which(this_delta>0)
  this_delta <- this_delta[nn]
    f_i <- which( unique_leaf_train == leaf_testing[i,2])
    # As shown below, the empirical distribution within each terminal node,f, is obtained by running "2. empirical distribution of terminal nodes.R"
    this_p <-  pkde(as.numeric(this_delta), fhat = f[[f_i]])
    this_p <- c(this_p[this_p < 1],1)
   prob <- c(prob,list(this_p))
 }

# For each passenger, record the first 15 time interval that is after a passenger's on-chock time (time of arriving at the airport), and the number of 15 min intervals thereafter when this_p < 1. This is mainly for later convenience and reducing calculation time (e.g., probability of arriving at the conference desk before on-chock time is always 0).
pax_firstperiod <- rep(NA, nrow(data))
pax_lastperiod <- rep(NA,nrow(data))
for (i in 1:nrow(data)) {
  this_delta <- difftime(df, ymd_hms(data$on_chocks_time[i]), units="mins")
  nn <- which(this_delta>0)
  pax_firstperiod[i] <- nn[1]
  pax_lastperiod[i] <- nn[1] + length(prob[[i]])-1
}

data_periods <- data.frame('first' = pax_firstperiod, 'pred_last' = pax_lastperiod)

# write.csv(data_periods,'first_last_periods_empirical_domestic.csv', row.names=FALSE)

# Calculate each passenger's probability of arriving within each of the 15 minutes interval (calculate PDF). 
prob_p <- c()
for (i in 1:length(prob)) {
  prob_p <- c(prob_p, list(diff(prob[[i]])))
}

# Recreate a list "prob_b" that records each passenger's probability of arriving before the first 15 min time interval, and probabilities of arriving within each 15 min time interval.
for (i in 1:length(prob)) {
  prob_p[[i]] <- c(prob[[i]][1], prob_p[[i]])
}

# save prob_p into an rds file for later use.
#saveRDS(prob_p, 'empirical_pdf_domestic.rds')

mean_vec <- rep(NA,length(df)-1)
var_vec <- rep(NA,length(df)-1)
for (i in 2:(length(df))) { # for each 15 min interval
  pax_n_this_period <- which(data_periods$first <= i&data_periods$pred_last >= i) # Find passengers with positive but < 1 probability of arriving in this interval.
  p_vec <- rep(NA,length(pax_n_this_period))
  if (length(pax_n_this_period) == 0)  next
     for (j in 1:length(pax_n_this_period)) { # For each of these passengers, calculate the probability of arriving within this interval.
       p_vec[j] <- prob_p[[pax_n_this_period[j]]][i+1-data_periods$first[pax_n_this_period[j]]]
     }
     p_vec[which(p_vec < 0)] = 0
     mean_vec[i-1] <- sum(p_vec) # Calculate the mean of the normal distribution. See section 3.5 in the paper.
     pax_thisperiod_flight <- data$ib_flight_no[pax_n_this_period]
    var_vec[i-1] <- sum(p_vec*(1-p_vec)) # Calculate the variance of the normal distribution
}
mean_vec[which(is.na(mean_vec))] = 0

# Calculate 5 quantiles
p_05 <- rep(0,length(df)-1)
p_25 <- rep(0,length(df)-1)
p_50 <- rep(0,length(df)-1)
p_75 <- rep(0,length(df)-1)
p_95 <- rep(0,length(df)-1)
for (i in 1:length(countPax1)) {
  if(is.na(mean_vec[i])) next
 p_05[i] <- qnorm(0.05,mean_vec[i], sqrt(var_vec[i]))
 p_25[i] <- qnorm(0.25,mean_vec[i], sqrt(var_vec[i]))
 p_50[i] <- qnorm(0.5,mean_vec[i], sqrt(var_vec[i]))
 p_75[i] <- qnorm(0.75,mean_vec[i], sqrt(var_vec[i]))
 p_95[i] <- qnorm(0.95,mean_vec[i], sqrt(var_vec[i]))
}
p_05[which(is.na(p_05))] <- 0
p_25[which(is.na(p_25))] <- 0
p_50[which(is.na(p_50))] <- 0
p_75[which(is.na(p_75))] <- 0
p_95[which(is.na(p_95))] <- 0
mean_vec[which(is.na(var_vec))] <- 0
var_vec[which(is.na(var_vec))] <- 0

p_05 <- c(0,p_05)
p_25 <- c(0,p_25)
p_50 <- c(0,p_50)
p_75 <- c(0,p_75)
p_95 <- c(0,p_95)
p_mean <- c(0,mean_vec)
var_vec <- c(0,var_vec)
countPax2 = read.csv('actual_flow_domestic.csv')[,1] # pre-saved actual passenger flows for domestic passengers.

# We only want to calculate accuracies during "peak" hours defined by Heathrow. Therefore, replace results during other periods with NA.
for (i in 1:(length(countPax1)/96-1)) {
  countPax2[((i-1)*96+89):(i*96+20)] <- NA
  p_mean[((i-1)*96+89):(i*96+20)] <- NA
  var_vec[((i-1)*96+89):(i*96+20)] <- NA
  p_05[((i-1)*96+89):(i*96+20)] <- NA
  p_25[((i-1)*96+89):(i*96+20)] <- NA
  p_50[((i-1)*96+89):(i*96+20)] <- NA
  p_75[((i-1)*96+89):(i*96+20)] <- NA
  p_95[((i-1)*96+89):(i*96+20)] <- NA
}
countPax2[((length(countPax1)/96-1)*96+89):length(countPax1)] <- NA
p_mean[((length(countPax1)/96-1)*96+89):length(countPax1)] <- NA
var_vec[((length(countPax1)/96-1)*96+89):length(countPax1)] <- NA
p_05[((length(countPax1)/96-1)*96+89):length(countPax1)] <- NA
p_25[((length(countPax1)/96-1)*96+89):length(countPax1)] <- NA
p_50[((length(countPax1)/96-1)*96+89):length(countPax1)] <- NA
p_75[((length(countPax1)/96-1)*96+89):length(countPax1)] <- NA
p_95[((length(countPax1)/96-1)*96+89):length(countPax1)] <- NA

countPax2[1:20] <- NA
p_mean[1:20] <- NA
var_vec[1:20] <- NA
p_05[1:20] <- NA
p_25[1:20] <- NA
p_50[1:20] <- NA
p_75[1:20] <- NA
p_95[1:20] <- NA

# Remove NAs so that only peak hours results are left.
countPax2 = na.omit(countPax2)
p_mean = na.omit(p_mean)
var_vec = na.omit(var_vec)
p_05 = na.omit(p_05)
p_25 = na.omit(p_25)
p_50 = na.omit(p_50)
p_75 = na.omit(p_75)
p_95 = na.omit(p_95)

# Define pinball function
pinball <- function(pred, act, q) {
  I = 1*(act>pred)
  l <- (act - pred)*q*I + (pred - act)*(1-q)*(1-I)
  return(mean(l))
}

# Print out results
c(pinball(p_05, countPax2, 0.05),
  pinball(p_25, countPax2, 0.25),
  pinball(p_50, countPax2, 0.50),
  pinball(p_75, countPax2, 0.75),
  pinball(p_95, countPax2, 0.95))

# Logscore and CRPS
library(scoringRules)
crps_score <- rep(NA, length(countPax2))
logs_score <- rep(NA, length(countPax2))
var_vec[which(var_vec == 0)] <- 0.001
for (i in 1:length(countPax2)) {
  crps_score[i] <- crps(y = countPax2[i], family = 'normal', mean = p_mean[i], sd = sqrt(var_vec[i])) 
  logs_score[i] <- logs(y = countPax2[i], family = 'normal', mean = p_mean[i], sd = sqrt(var_vec[i]))
}
mean(crps_score)
mean(logs_score)

# Output PIT figures.
F_i <- rep(0,length(countPax2))
for (i in 1:length(countPax2)) {
  F_i[i] <- pnorm(countPax2[i], p_mean[i], sqrt(var_vec[i]))
}
hist(F_i)
