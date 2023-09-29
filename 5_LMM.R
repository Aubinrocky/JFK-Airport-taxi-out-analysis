setwd("D:/ESAMI POLI/Applied Statistics/PROJECT")

library(readr)
library(dplyr)
library(nlme)
model_df <- read_csv("model_df.csv", col_types = cols(...1 = col_skip()))

model_df$routeID <- as.factor(model_df$routeID)
model_df$Origin <- as.factor(model_df$Origin)
model_df$Operating_Airline <- as.factor(model_df$Operating_Airline)


# to make comparisons between model let's only keep a subset, otherwise we have memory and computation time issues
set.seed(2000)
model_df2 <- sample_n(model_df, 10000)
summary(model_df2)

MCorr.4 <- gls(log(totalTaxi) ~ Operating_Airline + ArrDelay + Origin + concurrent_TOT + ArrDelay:Origin +
                 ArrDelay:Operating_Airline,
               weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
               correlation = corCompSymm(form = ~1|routeID),
               data = model_df2)

summary(MCorr.4)


# Now, Linear Mixed Models: Random intercept, heteroscedastic residuals. using routeID


LMMCorr.4 <- lme(log(totalTaxi) ~ Operating_Airline + ArrDelay + Origin + concurrent_TOT + ArrDelay:Origin +
                 ArrDelay:Operating_Airline,
               weights = varConstPower(form =~ as.vector(ArrDelay)|Operating_Airline),
               correlation = corCompSymm(form = ~1|routeID),
               random = ~1|routeID,
               control =list(msMaxIter = 1000, msMaxEval = 1000),
               data = model_df2 )

summary(LMMCorr.4)

plot(ranef(LMMCorr.4))

#ordered plot:
library(lattice)
re = ranef(LMMCorr.4)
dat = data.frame(x= row.names(re),y=re[,attr(re,'effectName')])
dotplot(reorder(x,y)~y,data=dat) # -> THEY ARE SO SMALL THAT IT DOESNT MAKE SENSE TO INCLUDE IT!


anova(MCorr.4, LMMCorr.4) # LMM is marginally better than MCorr.4

# note: "correlation" and "random" must have the same grouping factor, so we can only use routeID


