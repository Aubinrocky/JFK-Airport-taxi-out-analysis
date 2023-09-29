################################################################################
############################# RANDOM EFFECTS ###################################
################################################################################
library(nlme)
library(readr)
library(corrplot)
library(lattice)
library(plot.matrix)
library(lme4)
library(insight)


############################ STARTING POINT ####################################

#We are using the dataset model_df2 created in the script "4_Correlation_Structure_v2"
str(model_df2)

#Chosen variance structure
MFinal.4 <- gls(log(totalTaxi) ~ Operating_Airline + ArrDelay + Origin + concurrent_TOT + ArrDelay:Origin +
                  ArrDelay:Operating_Airline,
                weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
                data = model_df2) #AIC 19418.63

#chosen correlation structure
MCorr.4 <- gls(log(totalTaxi) ~ Operating_Airline + ArrDelay + Origin + concurrent_TOT + ArrDelay:Origin +
                  ArrDelay:Operating_Airline,
                weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
                correlation = corCompSymm(form = ~1|routeID),
                data = model_df2) #AIC 10324.76

#formula
form <- formula(log(totalTaxi) ~ Operating_Airline + ArrDelay + Origin + concurrent_TOT + ArrDelay:Origin +
                  ArrDelay:Operating_Airline)


############################ RANDOM EFFECTS ####################################

##### RANDOM INTERCEPT ##################################

#Let's introduce a random intercept.
#From a theoretical point of view we can try to use the same variables that we 
#can try to use the same variables that we tested for the correlation structure:

# 1 - Origin
# 2 - DayOfWeek
# 3 - month
# 4 - routeID
# 5 - NUMBER_OF_SEATS
# 6 - age
# 7 - FlightDate


# 1 - Origin
ctrl <- lmeControl(opt='optim')
MRe.1 <- lme(form, 
              weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
              random = ~1|Origin, data = model_df2, control = ctrl)

summary(MRe.1) #AIC 20213.5, it's higher than the one of MFinal.4, we don't like it


# 2 - DayOfWeek
MRe.2 <- lme(form, 
             weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
             random = ~1|DayOfWeek, data = model_df2, control = ctrl)

summary(MRe.2) #AIC 20187.39, it's higher than the one of MFinal.4, we don't like it


# 3 - month
MRe.3 <- lme(form, 
             weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
             random = ~1|month, data = model_df2, control = ctrl)

summary(MRe.3) #AIC 19837.29, it's higher than the one of MFinal.4, we don't like it

# 4 - routeID
MRe.4 <- lme(form, 
             weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
             random = ~1|routeID, data = model_df2, control = ctrl)

summary(MRe.4) #AIC 11292.06, 17813.65
#For now is the best one in  term of random effects

# 5 - NUMBER_OF_SEATS
MRe.5 <- lme(form, 
             weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
             random = ~1|NUMBER_OF_SEATS, data = model_df2, control = ctrl)

summary(MRe.5) #AIC 17813.65, lower than MFinal.4 but much higher than MCorr.4

# 6 - age
MRe.6 <- lme(form, 
             weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
             random = ~1|age, data = model_df2, control = ctrl)

summary(MRe.6) #AIC  19651.94, it's higher than the one of MFinal.4, we don't like it 



# The best random intercept is the one given by the use of routeID as grouping factor
# However the AIC is not as good as the one of the mdel that uses a correlation
# structure instead of a random intercept

MRe.4 <- lme(form, 
             weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
             random = ~1|routeID, data = model_df2, control = ctrl)

summary(MRe.4) 
#AIC 11292.06
#std. dev. of the random intercept: 0.1137583
#number of groups: 238


#Let's check the confidence intervals
intervals(MRe.4, which = "fixed")
intervals(MRe.4, which = "var-cov") 
#Error: Non-positive definite approximate variance-covariance
#il fatto che non funzioni mi fa presagir che
#ci sia qualcosa di sbagliato a livello teorico

#Let's check the plot of the random effect
dotplot(ranef(MRe.4)) #there is clearly a problem



