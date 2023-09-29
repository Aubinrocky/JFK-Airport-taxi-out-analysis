################################################################################
######################## CORRELATION STRUCTURES ################################
################################################################################

library(nlme)
library(readr)



############################ STARTING POINT ####################################


#We are using the dataset taxiTime_df created in the script "2_Model_Selection"
str(taxiTime_df)

# We create a second version of the dataset "model_df" (used in the script 
# "3_Variance_Structure") and we add some variable to better understand what we 
# have to use in the correlation structure.
# More in detail we add the following variables to the one already contained:
# "FlightDate", "Dest", "DayOfWeek", "NUMBER_OF_SEATS", "age", "month"


model_df2 <- taxiTime_df %>% select(c("routeID", "Distance", "Operating_Airline", "ArrDelay", "totalTaxi", 
                                      "concurrent_TOT", "Origin", "FlightDate", "Dest", "DayOfWeek",
                                      "NUMBER_OF_SEATS", "age", "month" ))


model_df2$Distance <- scale(model_df2$Distance)
model_df2$ArrDelay <- scale(model_df2$ArrDelay)
model_df2$concurrent_TOT <- scale(model_df2$concurrent_TOT)


# We have seen that the best model in terms of VARIANCE STRUCTURE is the one
# that takes into consideration the Delay of the plane and the Operating airline,
# with a varConstPower structure.
# The variance increase as the delay increase, at the same time we have different 
# powers for different Operating Airlines


MFinal.4 <- gls(log(totalTaxi) ~ Operating_Airline + ArrDelay + Origin + concurrent_TOT + ArrDelay:Origin +
                  ArrDelay:Operating_Airline,
                weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
                data = model_df2
                )

summary(MFinal.4) #AIC 19418.63


###################### CORRELATION STRUCTURES ##################################

# Let's introduce the correlation structures.
# From a theoretical point of view, the taxitime of planes can be correlated to
# several variables of the dataset:
# there may be correlation between taxit of planes departing from the same airport 
# -> grouping factor : "Origin"
# there may be correlation between taxitime and the day of the week or the month
# -> grouping factor: "DayOfWeek" or "Month"
# there may be correlation between taxi time of flights with the same routeID
# -> grouping factor: "route ID" (routeID as a sort of subject)
# there may be correlation between taxitime and the size or the age of the planes
# -> grouping factor: "NUMBER_OF_SEATS" or "age"
# there may be correlation between taxitime of planes departing on the same day
# -> grouping factor: "FlightDate"



# We will start from a simple correlation structure usung as grouping factor:
# 1 - Origin
# 2 - DayOfWeek
# 3 - month
# 4 - routeID
# 5 - NUMBER_OF_SEATS
# 6 - age
# 7 - FlightDate


# 1 - Origin
str(model_df2$Origin) # Factor w/ 3 levels

MCorr.1 <- gls(log(totalTaxi) ~ Operating_Airline + ArrDelay + Origin + concurrent_TOT + ArrDelay:Origin +
                 ArrDelay:Operating_Airline,
               weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
               correlation = corCompSymm(form = ~1|Origin),
               data = model_df2)
# Error: cannot allocate vector of length 987509190
# The groups are too few (3) compared to the number of observation (54.4222)
# We can't use this correlation structure



#2 - DayOfWeek
str(model_df2$DayOfWeek) # Factor w/ 7 levels
MCorr.2 <- gls(log(totalTaxi) ~ Operating_Airline + ArrDelay + Origin + concurrent_TOT + ArrDelay:Origin +
                 ArrDelay:Operating_Airline,
               weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
               correlation = corCompSymm(form = ~1|DayOfWeek),
               data = model_df2)
# Error: cannot allocate vector of size 3.2 Gb
# The groups are too few (7) compared to the number of observation (54.4222)
# We can't use this correlation structure



#3 - month
str(model_df2$month) # Factor w/ 12 levels
MCorr.3 <- gls(log(totalTaxi) ~ Operating_Airline + ArrDelay + Origin + concurrent_TOT + ArrDelay:Origin +
                 ArrDelay:Operating_Airline,
               weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
               correlation = corCompSymm(form = ~1|month),
               data = model_df2)
# Error: cannot allocate vector of size 1.8 Gb
# The groups are too few (12) compared to the number of observation (54.4222)
# We can't use this correlation structure


#4 - routeID
str(model_df2$routeID)
MCorr.4 <- gls(log(totalTaxi) ~ Operating_Airline + ArrDelay + Origin + concurrent_TOT + ArrDelay:Origin +
                 ArrDelay:Operating_Airline,
               weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
               correlation = corCompSymm(form = ~1|routeID),
               data = model_df2)
  
  # Let's check the AIC and the rho estimation
  summary(MCorr.4)
  #AIC: 10324.76
  #Rho: 0.248

  # To see if makes sense to keep the correlation structure:
  intervals(MCorr.4)
  #we check the confidence intervals of the parameter Rho, we see that:
  #Rho: lower: 0.2048466, est: 0.2483508, upper: 0.2976052
  #we can notice that 0 is not included in the interval, as a consequence
  #we can say that we should keep the correlation structure in the model
  
  # To see if the mode is better than the previous one (MFinal.4):
  anova(MFinal.4, MCorr.4)
  #we can say that MCorr.4 is significally better than the previous one
  #(the p-value of the test is very low <.0001, we have evidence to refuse H0)
  #we can also see that the AIC halves with the introduction of the correlation
  #structure in the model.
  #We choose MCorr.4
  
  # check the residuals
  plot(MCorr.4, resid(., type = "response") ~ ArrDelay)
  #there is still a bit of heteroscedasticity in the data 
  plot(MCorr.4, resid(., type = "pearson") ~ ArrDelay)
  #the situation improve if we consider the pearson residuals 
  plot(MCorr.4, resid(., type = "pearson" ) ~ fitted(.))
  #pearson residuals of fitted values seems homoscedastic
  plot(MCorr.4)

  
  
# 5 - NUMBER_OF_SEATS
str(model_df2$NUMBER_OF_SEATS) # numerical
MCorr.5 <- gls(log(totalTaxi) ~ Operating_Airline + ArrDelay + Origin + concurrent_TOT + ArrDelay:Origin +
                 ArrDelay:Operating_Airline,
               weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
               correlation = corCompSymm(form = ~1|NUMBER_OF_SEATS),
               data = model_df2)
# Error: cannot allocate vector of size 1.5 Gb
# we can try to use the variable as a factor


NUMBER_OF_SEATS.f = as.factor(model_df2$NUMBER_OF_SEATS)
str(NUMBER_OF_SEATS.f) # Factor w/ 41 levels
MCorr.5.1 <- gls(log(totalTaxi) ~ Operating_Airline + ArrDelay + Origin + concurrent_TOT + ArrDelay:Origin +
                   ArrDelay:Operating_Airline,
                 weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
                 correlation = corCompSymm(form = ~1|NUMBER_OF_SEATS.f),
                 data = model_df2)
# Error: cannot allocate vector of size 1.5 Gb
# The groups are too few (41) compared to the number of observation (54.4222)
# We can't use this correlation structure



# 6 - age
str(model_df2$age) # numerical
MCorr.6 <- gls(log(totalTaxi) ~ Operating_Airline + ArrDelay + Origin + concurrent_TOT + ArrDelay:Origin +
                 ArrDelay:Operating_Airline,
               weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
               correlation = corCompSymm(form = ~1|age),
               data = model_df2)
# Error: cannot allocate vector of size 1.1 Gb
# we can try to use the variable as a factor

age.f = as.factor(model_df2$age) 
str(age.f) # Factor w/ 30 levels
MCorr.6.1 <- gls(log(totalTaxi) ~ Operating_Airline + ArrDelay + Origin + concurrent_TOT + ArrDelay:Origin +
                   ArrDelay:Operating_Airline,
                 weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
                 correlation = corCompSymm(form = ~1|age.f),
                 data = model_df2)
# Error: cannot allocate vector of size 1.5 Gb
# The groups are too few (30) compared to the number of observation (54.4222)
# We can't use this correlation structure


# 7 - FlightDate
str(model_df2$FlightDate) #date (factor with 365 levels)
MCorr.7 <- gls(log(totalTaxi) ~ Operating_Airline + ArrDelay + Origin + concurrent_TOT + ArrDelay:Origin +
                 ArrDelay:Operating_Airline,
               weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
               correlation = corCompSymm(form = ~1|FlightDate),
               data = model_df2)


  # check the summary:
  summary(MCorr.7)
  #AIC 18796.23, is lower than the AIC of MFinal.4 but higher than MCorr.4
  #Rho 0.024, is very low, we don't like it
  
  # To see if makes sense to keep the correlation structure:
  intervals(MCorr.7)
  #Rho: lower: 0.02023292, est: 0.02444135, upper: 0.0293264
  #0 is not included, good
  
  # To see if the mode is better than the previous one (MCorr.4):
  anova(MFinal.4, MCorr.7)
  #the model is significally better than MFinal.4 (pval <.0001)
  anova(MCorr.4, MCorr.7)
  #the AIC of model MCorr.4 is lower
  #We keep the model MCorr.4

  
# Since in the dataset we have not variable that can be used as position
# variables, we can't perform models with more complex Correlation Structures
# (such as CorAR1 or CorSymm)
# For this reason we keep CorCompSymm as correlation structure

############################## CONCLUSIONS #####################################

# The best model is MCorr.4 that uses routeID as grouping factor
# The routeID is what identifies our subjects 
  

  MCorr.4 <- gls(log(totalTaxi) ~ Operating_Airline + ArrDelay + Origin + concurrent_TOT + ArrDelay:Origin +
                   ArrDelay:Operating_Airline,
                 weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
                 correlation = corCompSymm(form = ~1|routeID),
                 data = model_df2)

  summary(MCorr.4) # AIC 10324.76


