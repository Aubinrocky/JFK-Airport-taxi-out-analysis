####################################################
############## VARIANCE STRUCTURES #################
####################################################

setwd("D:/ESAMI POLI/Applied Statistics/PROJECT")

library(nlme)
library(readr)

model_df <- read_csv("model_df.csv", col_types = cols(...1 = col_skip()))

model_df$Distance <- scale(model_df$Distance)
model_df$ArrDelay <- scale(model_df$ArrDelay)
model_df$concurrent_TOT <- scale(model_df$concurrent_TOT)
model_df$routeID <- as.factor(model_df$routeID)
model_df$Operating_Airline <- as.factor(model_df$Operating_Airline)
model_df$Origin <- as.factor(model_df$Origin)

summary(model_df)

# model used: model 4

model_formulation4 <- formula(log(totalTaxi) ~ Operating_Airline+ArrDelay+Distance+Origin+concurrent_TOT+
                                Operating_Airline:ArrDelay + ArrDelay:Origin)

lm4 <- gls(model_formulation4, data = model_df)

summary(lm4)


# Some visual analysis:

bwplot(resid(lm4, type='response') ~ Operating_Airline,      
       pch = "|", data = model_df) # probably quite similar in terms of variance structure 

bwplot(resid(lm4, type='response') ~ Origin,      
       pch = "|", data = model_df) # probably quite similar in terms of variance structure 

bwplot(resid(lm4, type='response') ~ Operating_Airline | Origin,      
       pch = "|", data = model_df) # probably nothing, but we might want to test it anyways.

plot(model_df$ArrDelay, residuals(lm4)) # there might be an increase in residuals variance when ArrDelay increases

plot(model_df$Distance, residuals(lm4)) # probably nothing

plot(model_df$concurrent_TOT, residuals(lm4)) # smaller variance for lower and hgher concurrent_TOT values. Might be Interesting. 

plot(lm4)


# VARIANCE STRUCTURE 1: we check if variance structure depends on the airline. We expect that it doesnt though

M1 <- gls(model_formulation4,                  
             weights = varIdent(form = ~1|Operating_Airline), 
             data = model_df)

summary(M1)

anova(lm4, M1) # but actually it's significantly better

# VARIANCE STRUCTURE 2: we check if variance structure depends on the origin airport: 

M2 <- gls(model_formulation4,                  
          weights = varIdent(form = ~1|Origin), 
          data = model_df)

summary(M2)

anova(lm4, M1, M2) # M2 is worse than M1

# VARIANCE STRUCTURE 3: we check if variance structure depends also on ArrDelay 

M3 <- update(M2,                    
                weights = varPower(form = ~as.vector(ArrDelay)))


summary(M3)

anova(lm4, M1, M3) # as expected, this is much better!

# VARIANCE STRUCTURE 4: Instead of using varPower probably it's better to use varConstPower because ArrDelay has values equal or close 
# to zero

M4 <- update(M2,                    
             weights = varConstPower(form =~ as.vector(ArrDelay)))

summary(M4)

anova(lm4, M1, M3, M4) # as expected again, this last model 4 is the best one by far:

anova(M3,M4) # let's always use varConstPower instead of varPower!

# VARIANCE STRUCTURE 5: I want to use again varPower, but substituting ArrDelay with concurrent_TOT:

M5 <- update(M2,                    
             weights = varConstPower(form =~ as.vector(concurrent_TOT)))

summary(M5)

anova(lm4, M1, M3, M4, M5) # no, this is very bad. We stick with ArrDelay. 

# VARIANCE STRUCTURE 6: we add also Operating Airline to M4:

M6 <- update(M2,                    
             weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline))

summary(M6)

anova(lm4, M1, M3, M4, M5, M6) # And with this model 6 we improve again. Let's test whether adding Operating Airline is significant

anova(M4, M6) # it is significant so we keep M6 as the best variance structure. 

# VARIANCE STRUCTURE 7: using fitted values: 

M7 <- update(M2,                    
             weights = varConstPower(fixed=1))

summary(M7)

anova(lm4, M1, M3, M4, M5, M6, M7) # no improvements, M6 was better, and M3 with ArrDelay only was better also.

##################################################################################
# RESIDUALS ANALYSIS ON THE BEST MODEL (M6): 

# SCATTERPLOT of raw residuals vs. fitted
plot(M6, resid(., type = "response") ~ fitted(.))  # Raw residuals vs fitted. Looks kind of nice. 

# SCATTERPLOT  of raw residuals vs. ArrDelay
plot(M6, resid(., type = "response") ~ ArrDelay) # to be honest there is still heteroscedasticity

# BOXPLOTS of residuals to check if variance increases with airlines:
bwplot(resid(M6, type='response') ~ Operating_Airline,        
       pch = "|", data = model_df)  # I see homoscedasticity here

# PEARSON RESIDUALS: they should be more homoscedastic than raw residuals: 
plot(M6, resid(., type = "pearson" ) ~ fitted(.)) # Pearson vs fitted. Quite homoscedastic

# same plots as before, but with pearson residuals
plot(M6, resid(., type = "pearson") ~ ArrDelay)       # Pearson vs ArrDelay. Not so homoscedastic



# SCALE-LOCATION PLOTS = These are the scatterplots of the square-root transformation of the absolute value of the residuals versus 
# fitted values. The plots allow for detection of patterns in the residual variance.

# raw: 
plot(M6, sqrt(abs(resid(., type = "response"))) ~ fitted(.),
     type = c("p", "smooth"))
# seems nice to me

# Pearson: 
plot(M6, sqrt(abs(resid(., type = "pearson"))) ~ fitted(.),
     type = c("p", "smooth"))
# does not indicate any clear trend in the residual variance


# normalized residuals plotted separately by some variable
E2 <- resid(M6, type = "normalized")
coplot(E2 ~ ArrDelay | Operating_Airline, data = model_df, ylab = "Normalised residuals") # I still think they are heteroscedastic


# FINALLY: VARIANCE STRUCTURE CHOSEN.

MFinal <- gls(model_formulation4,                  
          weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline), 
          data = model_df,
          method = "ML")

summary(MFinal)

plot(M6)


# CHECKING AGAIN SIGNIFICANCE OF INTRODUCING TERMS IN THE MODEL:

# check 1: ArrDelay:Origin
MFinal.1 <- gls(log(totalTaxi) ~ Operating_Airline+ArrDelay+Distance+Origin+concurrent_TOT+
               Operating_Airline:ArrDelay,
             weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
             data = model_df,
             method = "ML")

anova(MFinal, MFinal.1) # low pval -> Keep ArrDelay:Origin


# check 2: Operating_Airline:ArrDelay
MFinal.2 <- gls(log(totalTaxi) ~ Operating_Airline+ArrDelay+Distance+Origin+concurrent_TOT + ArrDelay:Origin,
                weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
                data = model_df,
                method = "ML")

anova(MFinal, MFinal.2) # low pval -> Keep ArrDelay:Origin


# check 3: concurrent_TOT
MFinal.3 <- gls(log(totalTaxi) ~ Operating_Airline+ArrDelay+Distance+Origin + ArrDelay:Origin +
                  ArrDelay:Operating_Airline,
                weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
                data = model_df,
                method = "ML")

anova(MFinal, MFinal.3) # keep it


# check 4: Distance
MFinal.4 <- gls(log(totalTaxi) ~ Operating_Airline + ArrDelay + Origin + concurrent_TOT + ArrDelay:Origin +
                  ArrDelay:Operating_Airline,
                weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
                data = model_df,
                method = "ML")

anova(MFinal, MFinal.4) # keep it



#############################################################################
library(dplyr)
model_df_downsample <- sample_n(model_df, 10000)

summary(model_df_downsample)

M6.new1 <- update(M6,                    
                 correlation = corCompSymm(form = ~1|routeID),
                 data=model_df_downsample)

summary(M6.new1)


M6.new2 <- update(M6,                    
                  correlation = corSymm(form = ~1|routeID),
                  data=model_df_downsample)

summary(M6.new2)

AIC(M6, M6.new1, M6.new2)