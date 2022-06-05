# This file illustrates how we evaluate the improved predictions' impact on decision making as shown in Section 6 in the paper. Here we focus on the decision-making problem at the immigration area where only international passenger flows are used. Note that domestic passengers do not need to go through immigration desks. Please see Section 6 (Eq. 1 and 2) in the paper for more details.

quant <- c(0.05,0.25,0.5,0.75,0.95)

total_breaches <- matrix(NA, length(quant),6)
colnames(total_breaches) <- c('static','dynamic','tbats','arima','two-phased rt', 'two-phased lm')
resource_total <- matrix(NA, length(quant),6)
colnames(resource_total) <- c('static','dynamic','tbats','arima','two-phased rt', 'two-phased lm')

for (z in 1:5) { # for each of the five tested quantiles
  quant_select <- quant[z]
n_processed_perperiod <- c(12) # Assume 12 passengers can be processed within each 15 min interval.
mu <- n_processed_perperiod

# Calculate cu and cs in Eq (1) in the paper
cu <- 5
(cs <- (1 - quant_select)*cu*mu)

# ------------------------------
# Resource plan at immigration
# ------------------------------
resource_constraint <- c(100)

# Load in predictions.
actual_flow <- read.csv('actual_flow_domestic.csv')
static_flow <- read.csv('pax_flow_static_quantiles_domestic.csv')
dynamic_flow <- read.csv('pax_flow_dynamic_quantiles_domestic.csv')
tbats_flow <- read.csv('forecast_tbats_domestic.csv')
arima_flow <- read.csv('forecast_arima_domestic.csv')
arima_flow[arima_flow<0] <- 0
twophased_flow <- read.csv('pax_flow_twophased_rt_quantiles_domestic.csv')
twophased_flow_mean1 <-twophased_flow_mean <- read.csv('pax_flow_twophased_rt_domestic.csv')#
linear_flow <- read.csv('pax_flow_linear_quantiles_domestic.csv')
linear_flow_mean1 <- linear_flow_mean <- as.matrix(read.csv('pax_flow_linear_quantiles_domestic.csv')[,3])

# Find the point forecasts of the passenger flow.
static_flow_mean1 <- static_flow_mean <- as.matrix(static_flow[,3])
dynamic_flow_mean1 <- dynamic_flow_mean <- as.matrix(dynamic_flow[,3])
tbats_flow_mean1 <- tbats_flow_mean <- as.matrix(tbats_flow[,1])
arima_flow_mean1 <- arima_flow_mean <- as.matrix(arima_flow[,4])
arima_flow_mean1[is.na(arima_flow_mean1)] <- 0

static_flow <- as.matrix(static_flow[,z])
dynamic_flow <- as.matrix(dynamic_flow[,z])
tbats_flow <- as.matrix(tbats_flow[,z+1])
arima_flow <- as.matrix(arima_flow[,z+1])
twophased_flow <- as.matrix(twophased_flow[,z])
linear_flow <- as.matrix(linear_flow[,z])

actual_flow1 <- actual_flow <- read.csv('actual_flow_domestic.csv')
actual_flow_static <- static_flow
actual_flow_dynamic <- dynamic_flow
actual_flow_tbats <- tbats_flow
actual_flow_arima <- arima_flow
actual_flow_twophased <- twophased_flow
actual_flow_linear <- linear_flow
static_flow_plan_im <- as.matrix(rep(NA, nrow(static_flow)))
dynamic_flow_plan_im <- as.matrix(rep(NA, nrow(dynamic_flow)))
tbats_flow_plan_im <- as.matrix(rep(NA, nrow(tbats_flow)))
arima_flow_plan_im <- as.matrix(rep(NA, nrow(arima_flow)))
twophased_flow_plan_im <- as.matrix(rep(NA, nrow(twophased_flow)))
linear_flow_plan_im <- as.matrix(rep(NA, nrow(linear_flow)))
actual_not_processed_static <- as.matrix(rep(0, nrow(linear_flow)))
actual_not_processed_dynamic <- as.matrix(rep(0, nrow(linear_flow)))
actual_not_processed_tbats <- as.matrix(rep(0, nrow(linear_flow)))
actual_not_processed_arima <- as.matrix(rep(0, nrow(linear_flow)))
actual_not_processed_twophased <- as.matrix(rep(0, nrow(linear_flow)))
actual_not_processed_linear <- as.matrix(rep(0, nrow(linear_flow)))

# Calculate resourcing plans by using each competing model's predictions to make staffing decisions. See Section 6 for more details.
for (i in 2:nrow(actual_flow)) { # for each time period
  static_flow_plan_im[i,1] <- ( ((actual_flow_static[i,1])/n_processed_perperiod))
  cost1 <- cu*max((actual_flow_static[i,1] - floor(static_flow_plan_im[i,1])*n_processed_perperiod),0)+cs*floor(static_flow_plan_im[i,1])
  cost2 <- cu*max((actual_flow_static[i,1] - ceiling(static_flow_plan_im[i,1])*n_processed_perperiod),0)+cs*ceiling(static_flow_plan_im[i,1])
  if (cost1 <= cost2) {
    static_flow_plan_im[i,1] <- min(floor(static_flow_plan_im[i,1]),resource_constraint)
  } else {
    static_flow_plan_im[i,1] <- min(ceiling(static_flow_plan_im[i,1]),resource_constraint)
  }
  actual_not_processed_static[i,1] <-  max((actual_flow[i,1] - static_flow_plan_im[i,1]*n_processed_perperiod),0)
  pax_notprocessed_static <- max(actual_not_processed_static[i-1,1]+ static_flow_mean1[i,1] - static_flow_plan_im[i,1]*n_processed_perperiod,0)
  if (pax_notprocessed_static > 0 & i < nrow(actual_flow)) {
    actual_flow_static[i+1,1] <- actual_flow_static[i+1,1] + pax_notprocessed_static
    static_flow_mean[i+1,1] <- static_flow_mean1[i+1,1] + pax_notprocessed_static
  }
  if (actual_not_processed_static[i,1] > 0 & i < nrow(actual_flow)) {
    actual_flow[i+1,1] <- actual_flow[i+1,1] + actual_not_processed_static[i,1]
  }
  if (i%%96 == 0 & i < nrow(actual_flow)) { # Un-processed passengers will be cleared at the end of each day. They will not be carried to the next day.
    actual_flow_static[i+1,1] <- 0
    static_flow_mean[i+1,1] <- 0
    actual_flow[i+1,1] <- 0
  }
}

  actual_flow <- read.csv('actual_flow_domestic.csv')
  for (i in 2:nrow(actual_flow)) { 
  dynamic_flow_plan_im[i,1] <- ( ((actual_flow_dynamic[i,1])/n_processed_perperiod))
  cost1 <- cu*max((actual_flow_dynamic[i,1] - floor(dynamic_flow_plan_im[i,1])*n_processed_perperiod),0)+cs*floor(dynamic_flow_plan_im[i,1])
  cost2 <- cu*max((actual_flow_dynamic[i,1] - ceiling(dynamic_flow_plan_im[i,1])*n_processed_perperiod),0)+cs*ceiling(dynamic_flow_plan_im[i,1])
  if (cost1 <= cost2) {
    dynamic_flow_plan_im[i,1] <- min(floor(dynamic_flow_plan_im[i,1]),resource_constraint)
  } else {
    dynamic_flow_plan_im[i,1] <- min(ceiling(dynamic_flow_plan_im[i,1]),resource_constraint)
  }
  actual_not_processed_dynamic[i,1] <-  max((actual_flow[i,1] - dynamic_flow_plan_im[i,1]*n_processed_perperiod),0)
  pax_notprocessed_dynamic <- max(actual_not_processed_dynamic[i-1,1]+ dynamic_flow_mean1[i,1] - dynamic_flow_plan_im[i,1]*n_processed_perperiod,0)
  if (pax_notprocessed_dynamic > 0 & i < nrow(actual_flow)) {
    actual_flow_dynamic[i+1,1] <- actual_flow_dynamic[i+1,1] + pax_notprocessed_dynamic
    dynamic_flow_mean[i+1,1] <- dynamic_flow_mean[i+1,1] + pax_notprocessed_dynamic
  }
  if (actual_not_processed_dynamic[i,1] > 0 & i < nrow(actual_flow)) {
    actual_flow[i+1,1] <- actual_flow[i+1,1] + actual_not_processed_dynamic[i,1]
  }
  if (i%%96 == 0 & i < nrow(actual_flow)) {
    actual_flow_dynamic[i+1,1] <- 0
    dynamic_flow_mean[i+1,1] <- 0
    actual_flow[i+1,1] <- 0
  }
  
  } 
  
  actual_flow <- read.csv('actual_flow_domestic.csv')
  for (i in 2:nrow(actual_flow)) { 
  tbats_flow_plan_im[i,1] <- ( ((actual_flow_tbats[i,1])/n_processed_perperiod))
  cost1 <- cu*max((actual_flow_tbats[i,1] - floor(tbats_flow_plan_im[i,1])*n_processed_perperiod),0)+cs*floor(tbats_flow_plan_im[i,1])
  cost2 <- cu*max((actual_flow_tbats[i,1] - ceiling(tbats_flow_plan_im[i,1])*n_processed_perperiod),0)+cs*ceiling(tbats_flow_plan_im[i,1])
  if (cost1 <= cost2) {
    tbats_flow_plan_im[i,1] <- min(floor(tbats_flow_plan_im[i,1]),resource_constraint)
  } else {
    tbats_flow_plan_im[i,1] <- min(ceiling(tbats_flow_plan_im[i,1]),resource_constraint)
  }
  actual_not_processed_tbats[i,1] <-  max((actual_flow[i,1] - tbats_flow_plan_im[i,1]*n_processed_perperiod),0)
  pax_notprocessed_tbats <- max(actual_not_processed_tbats[i-1,1]+ tbats_flow_mean1[i,1] - tbats_flow_plan_im[i,1]*n_processed_perperiod,0)
  if (pax_notprocessed_tbats > 0 & i < nrow(actual_flow)) {
    actual_flow_tbats[i+1,1] <- actual_flow_tbats[i+1,1] + pax_notprocessed_tbats
    tbats_flow_mean[i+1,1] <- tbats_flow_mean[i+1,1] + pax_notprocessed_tbats
  }
  if (actual_not_processed_dynamic[i,1] > 0 & i < nrow(actual_flow)) {
    actual_flow[i+1,1] <- actual_flow[i+1,1] + actual_not_processed_dynamic[i,1]
  }
  if (i%%96 == 0 & i < nrow(actual_flow)) {
    actual_flow_tbats[i+1,1] <- 0
    tbats_flow_mean[i+1,1] <- 0
    actual_flow[i+1,1] <- 0
  }
  
  }
  
  actual_flow <- read.csv('actual_flow_domestic.csv')
  for (i in 2:nrow(actual_flow)) { 
    arima_flow_plan_im[i,1] <- ( ((actual_flow_arima[i,1])/n_processed_perperiod))
    cost1 <- cu*max((actual_flow_arima[i,1] - floor(arima_flow_plan_im[i,1])*n_processed_perperiod),0)+cs*floor(arima_flow_plan_im[i,1])
    cost2 <- cu*max((actual_flow_arima[i,1] - ceiling(arima_flow_plan_im[i,1])*n_processed_perperiod),0)+cs*ceiling(arima_flow_plan_im[i,1])
    if (cost1 <= cost2) {
      arima_flow_plan_im[i,1] <- min(floor(arima_flow_plan_im[i,1]),resource_constraint)
    } else {
      arima_flow_plan_im[i,1] <- min(ceiling(arima_flow_plan_im[i,1]),resource_constraint)
    }
    actual_not_processed_arima[i,1] <-  max((actual_flow[i,1] - arima_flow_plan_im[i,1]*n_processed_perperiod),0)
    pax_notprocessed_arima <- max(actual_not_processed_arima[i-1,1]+ arima_flow_mean1[i,1] - arima_flow_plan_im[i,1]*n_processed_perperiod,0)
    if (pax_notprocessed_arima > 0 & i < nrow(actual_flow)) {
      actual_flow_arima[i+1,1] <- actual_flow_arima[i+1,1] + pax_notprocessed_arima
      arima_flow_mean[i+1,1] <- arima_flow_mean1[i+1,1] + pax_notprocessed_arima
    }
    if (actual_not_processed_arima[i,1] > 0 & i < nrow(actual_flow)) {
      actual_flow[i+1,1] <- actual_flow[i+1,1] + actual_not_processed_arima[i,1]
    }
    if (i%%96 == 0 & i < nrow(actual_flow)) {
      actual_flow_arima[i+1,1] <- 0
      arima_flow_mean[i+1,1] <- 0
      actual_flow[i+1,1] <- 0
    }
    
  }
  
  actual_flow <- read.csv('actual_flow_domestic.csv')
  for (i in 2:nrow(actual_flow)) { 
  twophased_flow_plan_im[i,1] <- ( ((actual_flow_twophased[i,1])/n_processed_perperiod))
  cost1 <- cu*max((actual_flow_twophased[i,1] - floor(twophased_flow_plan_im[i,1])*n_processed_perperiod),0)+cs*floor(twophased_flow_plan_im[i,1])
  cost2 <- cu*max((actual_flow_twophased[i,1] - ceiling(twophased_flow_plan_im[i,1])*n_processed_perperiod),0)+cs*ceiling(twophased_flow_plan_im[i,1])
  if (cost1 <= cost2) {
    twophased_flow_plan_im[i,1] <- min(floor(twophased_flow_plan_im[i,1]),resource_constraint)
  } else {
    twophased_flow_plan_im[i,1] <- min(ceiling(twophased_flow_plan_im[i,1]),resource_constraint)
  }
  actual_not_processed_twophased[i,1] <-  max((actual_flow[i,1] - twophased_flow_plan_im[i,1]*n_processed_perperiod),0)
  pax_notprocessed_twophased <- max(actual_not_processed_twophased[i-1,1]+ twophased_flow_mean1[i,1] - twophased_flow_plan_im[i,1]*n_processed_perperiod,0)
  if (pax_notprocessed_twophased > 0 & i < nrow(actual_flow)) {
    actual_flow_twophased[i+1,1] <- actual_flow_twophased[i+1,1] + pax_notprocessed_twophased
    twophased_flow_mean[i+1,1] <- twophased_flow_mean[i+1,1] + pax_notprocessed_twophased
  }
  if (actual_not_processed_twophased[i,1] > 0 & i < nrow(actual_flow)) {
    actual_flow[i+1,1] <- actual_flow[i+1,1] + actual_not_processed_twophased[i,1]
  }
  if (i%%96 == 0 & i < nrow(actual_flow)) {
    actual_flow_twophased[i+1,1] <- 0
    twophased_flow_mean[i+1,1] <- 0
    actual_flow[i+1,1] <- 0
  }
  }
  
  actual_flow <- read.csv('actual_flow_domestic.csv')
  for (i in 2:nrow(actual_flow)) { 
    linear_flow_plan_im[i,1] <- ( ((actual_flow_linear[i,1])/n_processed_perperiod))
    cost1 <- cu*max((actual_flow_linear[i,1] - floor(linear_flow_plan_im[i,1])*n_processed_perperiod),0)+cs*floor(linear_flow_plan_im[i,1])
    cost2 <- cu*max((actual_flow_linear[i,1] - ceiling(linear_flow_plan_im[i,1])*n_processed_perperiod),0)+cs*ceiling(linear_flow_plan_im[i,1])
    if (cost1 <= cost2) {
      linear_flow_plan_im[i,1] <- min(floor(linear_flow_plan_im[i,1]),resource_constraint)
    } else {
      linear_flow_plan_im[i,1] <- min(ceiling(linear_flow_plan_im[i,1]),resource_constraint)
    }
    actual_not_processed_linear[i,1] <-  max((actual_flow[i,1] - linear_flow_plan_im[i,1]*n_processed_perperiod),0)
    pax_notprocessed_linear <- max(actual_not_processed_linear[i-1,1]+ linear_flow_mean1[i,1] - linear_flow_plan_im[i,1]*n_processed_perperiod,0)
    if (pax_notprocessed_linear > 0 & i < nrow(actual_flow)) {
      actual_flow_linear[i+1,1] <- actual_flow_linear[i+1,1] + pax_notprocessed_linear
      linear_flow_mean[i+1,1] <- linear_flow_mean[i+1,1] + pax_notprocessed_linear
    }
    if (actual_not_processed_linear[i,1] > 0 & i < nrow(actual_flow)) {
      actual_flow[i+1,1] <- actual_flow[i+1,1] + actual_not_processed_linear[i,1]
    }
    if (i%%96 == 0 & i < nrow(actual_flow)) {
      actual_flow_linear[i+1,1] <- 0
      linear_flow_mean[i+1,1] <- 0
      actual_flow[i+1,1] <- 0
    }
  }

# Calculate costs incurred by using each competing model's predictions to make staffing decisions. See Section 6 for more details.
actual_flow <- read.csv('actual_flow_domestic.csv')
breaches_static_im <- rep(0, nrow(static_flow_plan_im))
for (i in 2:nrow(static_flow_plan_im)) {
  pax_notprocessed <- actual_flow[i,1] - static_flow_plan_im[i,1]*n_processed_perperiod
  breaches_static_im[i] <- cu*max(0,pax_notprocessed)+cs*static_flow_plan_im[i,1]
  if (pax_notprocessed > 0& i < 7008) {
    if (static_flow_plan_im[i+1,1] == 0) static_flow_plan_im[i+1,1]<- static_flow_plan_im[i,1]
    actual_flow[i+1,1] <- actual_flow[i+1,1] + pax_notprocessed
  }
  if (i%%96 == 0& i < 7008) actual_flow[i+1,1] <- 0
}

for (i in 1:(nrow(static_flow_plan_im)/96-1)) {
  breaches_static_im[((i-1)*96+89):(i*96+20)] <- 0
}
breaches_static_im[((nrow(static_flow_plan_im)/96-1)*96+89):nrow(static_flow_plan_im)] <- 0

actual_flow <- read.csv('actual_flow_domestic.csv')
breaches_dynamic_im <- rep(0, nrow(dynamic_flow_plan_im))
for (i in 2:nrow(dynamic_flow_plan_im)) {
  pax_notprocessed <- actual_flow[i,1] - dynamic_flow_plan_im[i,1]*n_processed_perperiod
  breaches_dynamic_im[i] <- cu*max(0,pax_notprocessed)+cs*dynamic_flow_plan_im[i,1]
  if (pax_notprocessed > 0& i < 7008) {
    if (dynamic_flow_plan_im[i+1,1] == 0) dynamic_flow_plan_im[i+1,1]<- dynamic_flow_plan_im[i,1]
    actual_flow[i+1,1] <- actual_flow[i+1,1] + pax_notprocessed
  }
  if (i%%96 == 0& i < 7008) actual_flow[i+1,1] <- 0
}

for (i in 1:(nrow(dynamic_flow_plan_im)/96-1)) {
  breaches_dynamic_im[((i-1)*96+89):(i*96+20)] <- 0
}
breaches_dynamic_im[((nrow(dynamic_flow_plan_im)/96-1)*96+89):nrow(dynamic_flow_plan_im)] <- 0

actual_flow <- read.csv('actual_flow_domestic.csv')
breaches_tbats_im <- rep(0, nrow(tbats_flow_plan_im))
for (i in 2:nrow(tbats_flow_plan_im)) {
  pax_notprocessed <- actual_flow[i,1] - tbats_flow_plan_im[i,1]*n_processed_perperiod
  breaches_tbats_im[i] <- cu*max(0,pax_notprocessed)+cs*tbats_flow_plan_im[i,1]
  if (pax_notprocessed > 0& i < 7008) {
    if (tbats_flow_plan_im[i+1,1] == 0) tbats_flow_plan_im[i+1,1]<- tbats_flow_plan_im[i,1]
    actual_flow[i+1,1] <- actual_flow[i+1,1] + pax_notprocessed
  }
  if (i%%96 == 0& i < 7008) actual_flow[i+1,1] <- 0
}

for (i in 1:(nrow(tbats_flow_plan_im)/96-1)) {
  breaches_tbats_im[((i-1)*96+89):(i*96+20)] <- 0
}
breaches_tbats_im[((nrow(tbats_flow_plan_im)/96-1)*96+89):nrow(tbats_flow_plan_im)] <- 0

actual_flow <- read.csv('actual_flow_domestic.csv')
breaches_arima_im <- rep(0, nrow(arima_flow_plan_im))
for (i in 2:nrow(arima_flow_plan_im)) {
  pax_notprocessed <- actual_flow[i,1] - arima_flow_plan_im[i,1]*n_processed_perperiod
  breaches_arima_im[i] <- cu*max(0,pax_notprocessed)+cs*arima_flow_plan_im[i,1]
  if (pax_notprocessed > 0& i < 7008) {
    if (arima_flow_plan_im[i+1,1] == 0) arima_flow_plan_im[i+1,1]<- arima_flow_plan_im[i,1]
    actual_flow[i+1,1] <- actual_flow[i+1,1] + pax_notprocessed
  }
  if (i%%96 == 0& i < 7008) actual_flow[i+1,1] <- 0
}

for (i in 1:(nrow(arima_flow_plan_im)/96-1)) {
  breaches_arima_im[((i-1)*96+89):(i*96+20)] <- 0
}
breaches_arima_im[((nrow(arima_flow_plan_im)/96-1)*96+89):nrow(arima_flow_plan_im)] <- 0

actual_flow <- read.csv('actual_flow_domestic.csv')
breaches_twophased_im <- rep(0, nrow(twophased_flow_plan_im))
for (i in 2:nrow(twophased_flow_plan_im)) {
  pax_notprocessed <- actual_flow[i,1] - twophased_flow_plan_im[i,1]*n_processed_perperiod
  breaches_twophased_im[i] <- cu*max(0,pax_notprocessed)+cs*twophased_flow_plan_im[i,1]
  if (pax_notprocessed > 0& i < 7008) {
    if (twophased_flow_plan_im[i+1,1] == 0) twophased_flow_plan_im[i+1,1]<- twophased_flow_plan_im[i,1]
    actual_flow[i+1,1] <- actual_flow[i+1,1] + pax_notprocessed
  }
  if (i%%96 == 0& i < 7008) actual_flow[i+1,1] <- 0
}

for (i in 1:(nrow(twophased_flow_plan_im)/96-1)) {
  breaches_twophased_im[((i-1)*96+89):(i*96+20)] <- 0
}
breaches_twophased_im[((nrow(twophased_flow_plan_im)/96-1)*96+89):nrow(twophased_flow_plan_im)] <- 0

actual_flow <- read.csv('actual_flow_domestic.csv')
breaches_linear_im <- rep(0, nrow(linear_flow_plan_im))
for (i in 2:nrow(linear_flow_plan_im)) {
  pax_notprocessed <- actual_flow[i,1] - linear_flow_plan_im[i,1]*n_processed_perperiod
  breaches_linear_im[i] <- cu*max(0,pax_notprocessed)+cs*linear_flow_plan_im[i,1]
  if (pax_notprocessed > 0& i < 7008) {
    if (linear_flow_plan_im[i+1,1] == 0) linear_flow_plan_im[i+1,1]<- linear_flow_plan_im[i,1]
    actual_flow[i+1,1] <- actual_flow[i+1,1] + pax_notprocessed
  }
  if (i%%96 == 0& i < 7008) actual_flow[i+1,1] <- 0
}

for (i in 1:(nrow(linear_flow_plan_im)/96-1)) {
  breaches_linear_im[((i-1)*96+89):(i*96+20)] <- 0
}
breaches_linear_im[((nrow(linear_flow_plan_im)/96-1)*96+89):nrow(linear_flow_plan_im)] <- 0

total_breaches[z,] <- c(sum(breaches_static_im), sum(breaches_dynamic_im),sum(breaches_tbats_im), sum(breaches_arima_im),sum(breaches_twophased_im),sum(breaches_linear_im))
resource_total[z,] <- c(sum(static_flow_plan_im),sum(dynamic_flow_plan_im),sum(tbats_flow_plan_im),sum(arima_flow_plan_im),sum(twophased_flow_plan_im),sum(linear_flow_plan_im))


flowplan = cbind(tbats_flow_plan_im,arima_flow_plan_im, static_flow_plan_im,dynamic_flow_plan_im, twophased_flow_plan_im, linear_flow_plan_im)
flowplan[is.na(flowplan)] <- 0

# We are only interested in "peak" hours. So we replace results in other periods with NAs.
for (kk in 1:(7008/96-1)) {
  breaches_static_im[((kk-1)*96+89):(kk*96+20)] <- NA
  breaches_dynamic_im[((kk-1)*96+89):(kk*96+20)] <- NA
  breaches_tbats_im[((kk-1)*96+89):(kk*96+20)] <- NA
  breaches_arima_im[((kk-1)*96+89):(kk*96+20)] <- NA
  breaches_twophased_im[((kk-1)*96+89):(kk*96+20)] <- NA
  breaches_linear_im[((kk-1)*96+89):(kk*96+20)] <- NA
}
breaches_static_im[((7008/96-1)*96+89):7008] <- NA
breaches_dynamic_im[((7008/96-1)*96+89):7008] <- NA
breaches_tbats_im[((7008/96-1)*96+89):7008] <- NA
breaches_arima_im[((7008/96-1)*96+89):7008] <- NA
breaches_twophased_im[((7008/96-1)*96+89):7008] <- NA
breaches_linear_im[((7008/96-1)*96+89):7008] <- NA

breaches_static_im[1:20] <- NA
breaches_dynamic_im[1:20] <- NA
breaches_tbats_im[1:20] <- NA
breaches_arima_im[1:20] <- NA
breaches_twophased_im[1:20] <- NA
breaches_linear_im[1:20] <- NA

breaches_static_im <- na.omit(breaches_static_im)
breaches_dynamic_im <- na.omit(breaches_dynamic_im)
breaches_tbats_im <- na.omit(breaches_tbats_im)
breaches_arima_im <- na.omit(breaches_arima_im)
breaches_twophased_im <- na.omit(breaches_twophased_im)
breaches_linear_im <- na.omit(breaches_linear_im)

# Calculate improvements.
aa = (breaches_static_im - breaches_twophased_im)/breaches_static_im
aa = aa[which(aa > - Inf)]
improvements[3,z] <- mean(aa)
aa = (breaches_dynamic_im - breaches_twophased_im)/breaches_dynamic_im
aa = aa[which(aa > - Inf)]
improvements[4,z] <-mean(aa)
aa = (breaches_tbats_im - breaches_twophased_im)/breaches_tbats_im
aa = aa[which(aa > - Inf)]
improvements[1,z] <-mean(aa)
aa = (breaches_arima_im - breaches_twophased_im)/breaches_arima_im
aa = aa[which(aa > - Inf)]
improvements[2,z] <-mean(aa)
aa = (breaches_linear_im - breaches_twophased_im)/breaches_linear_im
aa = aa[which(aa > - Inf)]
improvements[6,z] <-mean(aa)
}
 improvements <- matrix(0,6,length(quant))
 improvements[3,] <- (total_breaches[,1]-total_breaches[,5])/total_breaches[,1]
 improvements[4,] <- (total_breaches[,2]-total_breaches[,5])/total_breaches[,2]
 improvements[1,] <- (total_breaches[,3]-total_breaches[,5])/total_breaches[,3]
 improvements[2,] <- (total_breaches[,4]-total_breaches[,5])/total_breaches[,4]
 improvements[6,] <- (total_breaches[,6]-total_breaches[,5])/total_breaches[,6]

rownames(improvements) <- c('tbats','arima','static','dynamic','two-phased rt','two-phased lm')
improvements[-5,]*100


