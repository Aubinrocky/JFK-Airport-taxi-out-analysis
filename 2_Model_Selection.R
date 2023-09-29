################################################################################
################## LET'S WORK ON OUR NEW DATASET! ##############################
################################################################################

setwd("D:/ESAMI POLI/Applied Statistics/PROJECT")

library(dplyr)
library(readr)
library(lattice)

sample_taxiTime <- read_csv("sample_taxiTime.csv", 
                            col_types = cols(...1 = col_skip(), FlightDate = col_date(format = "%Y-%m-%d"), 
                                             Origin = col_factor(levels = c("BOS","JFK", "PHX"))))

sample_taxiTime$routeID <- as.factor(sample_taxiTime$routeID)
sample_taxiTime$DepTimeBlk <- as.factor(sample_taxiTime$DepTimeBlk)
sample_taxiTime$ArrTimeBlk <- as.factor(sample_taxiTime$ArrTimeBlk)
sample_taxiTime$Dest <- as.factor(sample_taxiTime$Dest)
sample_taxiTime$DayOfWeek <- as.factor(sample_taxiTime$DayOfWeek)
sample_taxiTime$Operating_Airline <- as.factor(sample_taxiTime$Operating_Airline)
sample_taxiTime$month <- as.factor(sample_taxiTime$month)

sample_taxiTime$concurrent_TOT <- sample_taxiTime$Concurrent_Departure + sample_taxiTime$Concurrent_Arrival

sample_taxiTime <- sample_taxiTime %>% select(!c(TaxiIn, TaxiOut, Concurrent_Departure, Concurrent_Arrival))

summary(sample_taxiTime)

# before proceding further I want to get rid of outliers, at least for the most important variables. 

# starting from total_taxi: 

bwplot(sample_taxiTime$totalTaxi)

bwplot(sample_taxiTime$totalTaxi ~ sample_taxiTime$Operating_Airline )

bwplot(sample_taxiTime$totalTaxi ~ sample_taxiTime$Origin)

q_totalTaxi <- quantile(sample_taxiTime$totalTaxi, probs=c(.25, .75))
iqr_totalTaxi <- IQR(sample_taxiTime$totalTaxi)

low_totalTaxi <- as.numeric(q_totalTaxi[1]) - 1.5*iqr_totalTaxi
high_totalTaxi <- as.numeric(q_totalTaxi[2]) + 1.5*iqr_totalTaxi

taxiTime_df <- sample_taxiTime %>% filter( totalTaxi>low_totalTaxi & totalTaxi<high_totalTaxi) # around 2500 obs eliminated

# other variables that we'll likely use later in the model: TMAX, ArrDelay, concurrentTot. Let's check TMAX: 

bwplot(sample_taxiTime$TMAX)

bwplot(sample_taxiTime$TMAX ~ sample_taxiTime$Operating_Airline )

bwplot(sample_taxiTime$TMAX ~ sample_taxiTime$Origin) # no outliers.

# other variables that we'll likely use later in the model: TMAX, ArrDelay, concurrentTot. Let's check ArrDelay: 

bwplot(sample_taxiTime$ArrDelay)

bwplot(sample_taxiTime$ArrDelay ~ sample_taxiTime$Operating_Airline )

bwplot(sample_taxiTime$ArrDelay ~ sample_taxiTime$Origin)

q_delay <- quantile(sample_taxiTime$ArrDelay, probs=c(.25, .75))
iqr_delay <- IQR(sample_taxiTime$ArrDelay)

low_ArrDelay <- as.numeric(q_delay[1]) - 1.5*iqr_delay
high_ArrDelay <- as.numeric(q_delay[2]) + 1.5*iqr_delay

taxiTime_df <- sample_taxiTime %>% filter( ArrDelay > low_ArrDelay & ArrDelay < high_ArrDelay) # around 3000 obs eliminated

# other variables that we'll likely use later in the model: TMAX, ArrDelay, concurrentTot. Let's check concurrentTot

bwplot(sample_taxiTime$concurrent_TOT)

bwplot(sample_taxiTime$concurrent_TOT ~ sample_taxiTime$Operating_Airline )

bwplot(sample_taxiTime$concurrent_TOT ~ sample_taxiTime$Origin) # very few outliers and reasonable values so I will keep them all. 

summary(taxiTime_df)

################################################################################
############ RANDOM FOREST (BORUTA ALGORITHM) FOR FEATURE SELECTION ############
################################################################################

hist(taxiTime_df$totalTaxi)
hist(log(taxiTime_df$totalTaxi)) # -> Perfect. Let's keep this transformation. 

library(randomForest)
library(Boruta)

set.seed(69)

boruta_sample <- Boruta(log(totalTaxi) ~ ., data = taxiTime_df, maxRuns = 100)

boruta_sample$finalDecision

plot(boruta_sample)

boruta_sample$ImpHistory 

# top: Airline; ArrDelay; Origin; Distance; and maybe concurrent

# let's now try a simple model to see how it performs: 

model_formulation <- formula(log(totalTaxi) ~ Operating_Airline+ArrDelay+Distance+Origin+concurrent_TOT)

model_sample_lm <- lm(model_formulation, data = taxiTime_df)

summary(model_sample_lm) # this way we get R2 = 50.9%. GOOD!!!

# Now let's check also linear correlation: 

sample_numeric <- sample_taxiTime %>% select(c(CRSDepTime, DepDelay, ArrDelay, TMAX, PRCP, SNOW, AWND, Distance, 
                                               NUMBER_OF_SEATS, totalTaxi, age, concurrent_TOT))

library(psych)

corPlot(sample_numeric)

# and with log(totalTaxi):

sample_numeric_2 <- sample_numeric

sample_numeric_2$totalTaxi <- log(sample_numeric_2$totalTaxi)

corPlot(sample_numeric_2)

# MODEL FITTING: 
# now let's test two models: one complete and one without interactions. 
#################################################################################
# MODEL FITTING 1: model without interactions:

model_formulation1 <- formula(log(totalTaxi) ~ Operating_Airline+ArrDelay+Distance+Origin+concurrent_TOT)

lm1 <- lm(model_formulation1, data = taxiTime_df)

summary(lm1) # all variables are significant, r2 = 50.88%

plot(lm1$residuals ) #they look quite nice unfortunately

qqnorm(residuals(lm1))
qqline(residuals(lm1)) # Absolutely totally normal

plot(lm1$fitted.values, residuals(lm1), col = taxiTime_df$Origin) # residuals vs. fitted, color by Origin. Interesting, differences 
# exist with respect to origin.

plot(lm1$fitted.values, residuals(lm1), col = taxiTime_df$Operating_Airline) # residuals vs. fitted, color by Airline. Interesting,
# differences exist with respect to Airline.

################################################################################
# MODEL FITTING 2: Complete Model: 
model_formulation2 <- formula(log(totalTaxi) ~ Operating_Airline*ArrDelay*Distance*Origin*concurrent_TOT)

lm2 <- lm(model_formulation2, data = taxiTime_df)

summary(lm2) # much more complex, r2 = 52.78%, some interaction might be significant, but not all these. 
# Interactions that might be significant: Origin:concurrent_TOT; But let's check with anova:

anova(lm2) # we can try to add Operating_Airline:ArrDelay; ArrDelay:Origin. We do that in model 4.

###################################################################################
# MODEL FITTING 3: adding only the interaction Origin:Operating_Airline

model_formulation3 <- formula(log(totalTaxi) ~ Operating_Airline+ArrDelay+Distance+Origin+concurrent_TOT+Origin:Operating_Airline)

lm3 <- lm(model_formulation3, data = taxiTime_df)

summary(lm3) # it's significant, when it's not NA. R2 = 51.5%. 

plot(lm3$residuals ) #they look quite nice unfortunately

qqnorm(residuals(lm3))
qqline(residuals(lm3)) # Absolutely totally normal

plot(lm3$fitted.values, residuals(lm3), col = taxiTime_df$Origin) # residuals vs. fitted, color by Origin. Interesting, differences 
# exist with respect to origin.

plot(lm3$fitted.values, residuals(lm3), col = taxiTime_df$Operating_Airline) # residuals vs. fitted, color by Airline. Interesting,
# differences exist with respect to Airline.

plot(lm3)

# CHECKING IF WE NEED TO INCLUDE Origin:Airline

summary(lm3)

library(car)

linearHypothesis(lm3,
                 rbind(c(0,0,0,0,0,0,0,0,0,1,0,0,0), 
                       c(0,0,0,0,0,0,0,0,0,0,1,0,0),
                       c(0,0,0,0,0,0,0,0,0,0,0,1,0),
                       c(0,0,0,0,0,0,0,0,0,0,0,0,1)), 
                 c(0,0,0,0),
                 singular.ok = TRUE) # It's significant. NOTE: columns with NAs are skipped of course

anova(lm3) # Anova tells us that it's significant too

#################################################################################
# MODEL FITTING 4: adding Operating_Airline:ArrDelay; ArrDelay:Origin

model_formulation4 <- formula(log(totalTaxi) ~ Operating_Airline+ArrDelay+Distance+Origin+concurrent_TOT+
                                Operating_Airline:ArrDelay + ArrDelay:Origin)

lm4 <- lm(model_formulation4, data = taxiTime_df)

summary(lm4) # it's significant, when it's not NA. R2 = 51.5%. 

plot(lm4$residuals ) #they look quite nice unfortunately

qqnorm(residuals(lm4))
qqline(residuals(lm4)) # Absolutely totally normal

plot(lm4$fitted.values, residuals(lm4), col = taxiTime_df$Origin) # residuals vs. fitted, color by Origin. Interesting, differences 
# exist with respect to origin.

plot(lm4$fitted.values, residuals(lm4), col = taxiTime_df$Operating_Airline) # residuals vs. fitted, color by Airline. Interesting,
# differences exist with respect to Airline.

plot(lm4)

# CHECKING IF WE NEED TO INCLUDE Airline:Delay

summary(lm4)

library(car)

linearHypothesis(lm4,
                 rbind(c(0,0,0,0,0,0,0,0,0,1,0,0,0,0), 
                       c(0,0,0,0,0,0,0,0,0,0,1,0,0,0),
                       c(0,0,0,0,0,0,0,0,0,0,0,1,0,0)), 
                 c(0,0,0)) # low pval -> Include it!

anova(lm4) # anova confirms it

# CHECKING IF WE NEED TO INCLUDE Delay:Origin

summary(lm4)

library(car)

linearHypothesis(lm4,
                 rbind(c(0,0,0,0,0,0,0,0,0,0,0,0,1,0), 
                       c(0,0,0,0,0,0,0,0,0,0,0,0,0,1)), 
                 c(0,0)) # low pval -> Include it!

anova(lm4) # anova confirms it

# CHECKING IF WE NEED TO INCLUDE concurrent_TOT

summary(lm4)

library(car)

linearHypothesis(lm4,
                 rbind(c(0,0,0,0,0,0,0,0,1,0,0,0,0,0)), 
                 c(0)) # low pval -> Include it!

anova(lm4) # anova confirms it

# CHECKING IF WE NEED TO INCLUDE distance

summary(lm4)

library(car)

linearHypothesis(lm4,
                 rbind(c(0,0,0,0,0,1,0,0,0,0,0,0,0,0)), 
                 c(0)) # low pval -> Include it!

anova(lm4) # anova confirms it

# so we'll go on with model 4.
################################################################################
# MODEL FITTING 5: adding Operating_Airline:ArrDelay + ArrDelay:Origin + Origin:Operating_Airline

model_formulation5 <- formula(log(totalTaxi) ~ Operating_Airline+ArrDelay+Distance+Origin+concurrent_TOT+
                                Operating_Airline:ArrDelay + ArrDelay:Origin + Origin:Operating_Airline)

lm5 <- lm(model_formulation5, data = taxiTime_df)

summary(lm5) # it's the best one, with r2 = 52%, but I have doubts about the presence of NAs, so I'll continue with lm4 for now. 

AIC(lm1,lm2,lm3,lm4,lm5) # model 3 would be the best one (excluding model 2 which is far too complex), but I have doubts about including
# Airline:Origin, which generates NAs in the coefficients of the model. 
# So, for the moment I use lm4, but then if adding those NAs is ok, we can switch to lm3 or lm5

################################################################################
# VARIANCE STRUCTURE: EXPLORATIVE ANALYSIS OF RESIDUALS. Model: lm4

model_df <- taxiTime_df %>% select(c("routeID", "Distance", "Operating_Airline", "ArrDelay", "totalTaxi", "concurrent_TOT", 
                                     "Origin"))

model_df$Distance <- scale(model_df$Distance)
model_df$ArrDelay <- scale(model_df$ArrDelay)
model_df$concurrent_TOT <- scale(model_df$concurrent_TOT)

model_formulation4 <- formula(log(totalTaxi) ~ Operating_Airline + ArrDelay  + Distance + Origin + concurrent_TOT +
                                Operating_Airline:ArrDelay + ArrDelay:Origin)

lm4 <- lm(model_formulation4, data = model_df)

summary(lm4)

bwplot(resid(lm4, type='response') ~ Operating_Airline,      
       pch = "|", data = model_df) # probably quite similar in terms of variance structure 

bwplot(resid(lm4, type='response') ~ Origin,      
       pch = "|", data = model_df) # probably quite similar in terms of variance structure 

bwplot(resid(lm4, type='response') ~ Operating_Airline | Origin,      
       pch = "|", data = model_df) # probably nothing, but we might want to test it anyways.

plot(model_df$ArrDelay, residuals(lm4)) # there might be a small increase in residuals variance when ArrDelay increases

plot(model_df$Distance, residuals(lm4)) # probably nothing

plot(model_df$concurrent_TOT, residuals(lm4)) # smaller variance for lower and hgher concurrent_TOT values. Might be Interesting. 

plot(lm4)

#write.csv(model_df, "D:/ESAMI POLI/Applied Statistics/PROJECT/model_df.csv")

# ANALYSIS OF VARIANCE FOR RESIDUALS OF MDOEL %
# now: repeating the same thing for model 5, the best one


model_formulation5 <- formula(log(totalTaxi) ~ Operating_Airline+ArrDelay+Distance+Origin+concurrent_TOT+
                                Operating_Airline:ArrDelay + ArrDelay:Origin + Origin:Operating_Airline)

lm5 <- lm(model_formulation5, data = model_df)

summary(lm5)

bwplot(resid(lm5, type='response') ~ Operating_Airline,      
       pch = "|", data = model_df) # probably quite similar in terms of variance structure 

bwplot(resid(lm5, type='response') ~ Origin,      
       pch = "|", data = model_df) # probably quite similar in terms of variance structure 

bwplot(resid(lm5, type='response') ~ Operating_Airline | Origin,      
       pch = "|", data = model_df) # probably nothing, but we might want to test it anyways.

plot(model_df$ArrDelay, residuals(lm5)) # there might be a small increase in residuals variance when ArrDelay increases

plot(model_df$Distance, residuals(lm4)) # probably nothing

plot(model_df$concurrent_TOT, residuals(lm4)) # kind of an egg shape again 
