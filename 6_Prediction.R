setwd("D:/ESAMI POLI/Applied Statistics/PROJECT")

library(readr)
library(dplyr)
library(nlme)

model_df <- read_csv("model_df.csv", col_types = cols(...1 = col_skip()))

model_df$routeID <- as.factor(model_df$routeID)
model_df$Origin <- as.factor(model_df$Origin)
model_df$Operating_Airline <- as.factor(model_df$Operating_Airline)


low_totalTaxi <- -3.5
high_totalTaxi <- 131

low_ArrDelay <- -54
high_ArrDelay <- 42

# model: 

MCorr.4 <- gls(log(totalTaxi) ~ Operating_Airline + ArrDelay + Origin + concurrent_TOT + ArrDelay:Origin +
                 ArrDelay:Operating_Airline,
               weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
               correlation = corCompSymm(form = ~1|routeID),
               data = model_df) # AIC:  5781.538; rho = 0.5857969

# Test set: 

test_set <- read_csv("test_set.csv", col_types = cols(...1 = col_skip(), 
                                                           FlightDate = col_skip(), DepTimeBlk = col_skip(), 
                                                           ArrTimeBlk = col_skip(), Dest = col_skip(), 
                                                           CRSDepTime = col_skip(), DepDelay = col_skip(), 
                                                           DayOfWeek = col_skip(), TaxiOut = col_skip(), 
                                                           TaxiIn = col_skip(), AWND = col_skip(), 
                                                           PRCP = col_skip(), SNOW = col_skip(), 
                                                           TMAX = col_skip(), NUMBER_OF_SEATS = col_skip(), 
                                                           age = col_skip(), month = col_skip()))


summary(test_set)

test_set$concurrent_TOT <- test_set$Concurrent_Arrival + test_set$Concurrent_Departure

test_set <- test_set %>% select(-c("Concurrent_Arrival", "Concurrent_Departure"))

summary(test_set)

test_set$routeID <- as.factor(test_set$routeID)
test_set$Origin <- as.factor(test_set$Origin)
test_set$Operating_Airline <- as.factor(test_set$Operating_Airline)

summary(test_set)

# before going forward: we need to eliminate outliers and scale the variables. Keeping the same values used before: 


test_set <- test_set %>% filter( totalTaxi>low_totalTaxi & totalTaxi<high_totalTaxi) # 400 obs eliminated
test_set <- test_set %>% filter( ArrDelay > low_ArrDelay & ArrDelay < high_ArrDelay) # 700 obs eliminated (more or less)

test_set$Distance <- scale(test_set$Distance)
test_set$ArrDelay <- scale(test_set$ArrDelay)
test_set$concurrent_TOT <- scale(test_set$concurrent_TOT)

summary(test_set)
summary(model_df) # same distributions, so all looks good. 

test_set$Distance <- as.vector(test_set$Distance)
test_set$ArrDelay <- as.vector(test_set$ArrDelay)
test_set$concurrent_TOT <- as.vector(test_set$concurrent_TOT)

# and eliminating also the less frequent routes because it might be possible that we didnt have them in the model_df

route_freq_df <- test_set %>% group_by(routeID) %>% summarise(route_freq = n())

test_set <- merge(test_set, route_freq_df, by = "routeID")

test_set <- test_set %>% subset(route_freq > 9) # around 200 eliminated, we remain with 8678 observations.
test_set <- test_set %>% select(-c("route_freq"))

test_set <- droplevels(test_set)

table(test_set$routeID)

# predictions: 

test_set$prediction <- predict(MCorr.4, test_set)

test_set$prediction_exp <- exp(test_set$prediction)

plot(test_set$totalTaxi, test_set$prediction_exp) # let's put this one on the poster

# residuals: t-test to check if the mean is equal to 0

test_set$residual <- test_set$totalTaxi - test_set$prediction_exp

mean(test_set$residual*test_set$residual)

t.test(test_set$residual,mu=0)

model_df$prediction <- predict(MCorr.4, model_df)
model_df$prediction_exp <- exp(model_df$prediction)


model_df$residual <- model_df$totalTaxi - model_df$prediction_exp
t.test(model_df$residual,mu=0)

mean(model_df$residual*model_df$residual)
plot(model_df$totalTaxi, model_df$prediction_exp)

# test if test_df residuals have the same average of model_df residuals

t.test(test_set$residual,model_df$residual) # this is very good so we need to include it 100% on the poster

summary(test_set)
summary(model_df)

# RMSE:

library(Metrics)

rmseTest <- rmse(test_set$totalTaxi, test_set$prediction_exp)

rmseTrain <- rmse(model_df$totalTaxi, model_df$prediction_exp) # they are the same, very good again 

# plot of residuals of test set:

plot(test_set$totalTaxi, test_set$residual) # this is not so good so better not showing it







# ignore from now on

# model residuals plots

plot(MCorr.4, resid(., type = "response") ~ ArrDelay)
#there is still a bit of heteroscedasticity in the data 
plot(MCorr.4, resid(., type = "pearson") ~ ArrDelay)
#the situation improve if we consider the pearson residuals 
plot(MCorr.4, resid(., type = "pearson" ) ~ fitted(.))
#pearson residuals of fitted values seems homoscedastic
plot(MCorr.4)


# r2 of both training and test: 

R2 <- cor(model_df$totalTaxi,model_df$prediction_exp)^2
R2test <- cor(test_set$totalTaxi,test_set$prediction_exp)^2

R2train <- 1 - with(model_df, (sum((totalTaxi-prediction_exp)^2)/sum((totalTaxi-mean(totalTaxi))^2)))

R2test <- 1 - with(test_set, (sum((totalTaxi-prediction_exp)^2)/sum((totalTaxi-mean(totalTaxi))^2)))

rss <- sum((model_df$prediction_exp - model_df$totalTaxi) ^ 2) 
tss <- sum((model_df$totalTaxi - mean(model_df$totalTaxi)) ^ 2) 
rsq <- 1 - rss/tss
