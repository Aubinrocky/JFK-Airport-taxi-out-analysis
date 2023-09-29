###################################################################################
#################### DATASET CREATION #############################################
###################################################################################

setwd("D:/ESAMI POLI/Applied Statistics/PROJECT/RAW INITIAL DATASETS")

library(data.table)

df <- fread("Combined_Flights_2019.csv", select = c(1,3,4,7,10,14,16,21,26,27,30,43,50,51,54,55,56,59))

df_sample <- df[sample(nrow(df), 500000)]

library(dplyr)

taxitime_comparison <- df_sample %>% group_by(Origin) %>% summarise(avg_taxi_out = mean(TaxiOut, na.rm = TRUE),
                                                                    avg_taxi_in = mean(TaxiIn, na.rm = TRUE), 
                                                                    avg_tot_taxi = avg_taxi_in+avg_taxi_out, 
                                                                    n_obs = n())

# note: as expected, JFK is the 2nd airport in terms of tot_taxi time (34.36499 mins)
# Let's find another (big) airport that has instead a significantly lower tot_taxi time:

# BOS -> 28.43823 mins, and similar number of observations as JFK. 
# LAX -> 25.09798 mins, and a lot of observations. 
# MCO (Orlando) -> 23.62599 mins and similar number of observations as JFK.
# LAS (Las Vegas) -> 23.27846 mins, and similar number of observations as JFK.
# PHX (Phoenix) -> 21.38857 min, and similar number of observations as JFK.
# HOU (Houston) -> 16.404952 mins, but half of the obsrvations of JFK.

#Let's try this way: I keep 3 airports: 
# 1. JFK -> 34.4 mins
# 2. BOS -> 28.4 mins
# 3. PHX -> 21.4 mins

# In this way, origin airport should be a significant predictor of total taxi time. Now I re-build the initial dataset. 

rm(df_sample)
rm(taxitime_comparison)

################################################################################
################ LET'S PREPARE THE WEATHER DF FOR MERGING LATER ################ 
################################################################################

weather_df <- fread("airport_weather_2019.csv", select = c(2,3,4,6,7,10))

weather_df_3 <- subset(weather_df, NAME == "LAGUARDIA AIRPORT, NY US" | NAME == "BOSTON, MA US" | NAME == "PHOENIX AIRPORT, AZ US")

# In Phoenix it can't snow so snow values are NA. I substitute them with 0. 
weather_df_3[is.na(weather_df_3)] <- 0

weather_df_3$DATE <- as.Date(weather_df_3$DATE, "%m/%d/%Y")

weather_df_3$NAME[weather_df_3$NAME == "LAGUARDIA AIRPORT, NY US"] <- "JFK"
weather_df_3$NAME[weather_df_3$NAME == "BOSTON, MA US"] <- "BOS"
weather_df_3$NAME[weather_df_3$NAME == "PHOENIX AIRPORT, AZ US"] <- "PHX"

weather_df_3$NAME <- as.factor(weather_df_3$NAME)

colnames(weather_df_3)[1] <- "Origin"
colnames(weather_df_3)[2] <- "FlightDate"

summary(weather_df_3) # IT'S READY

################################################################################
################ LET'S PREPARE THE AIRCRAFT DATASET NOW ########################
################################################################################

airplanes_df <- fread("B43_AIRCRAFT_INVENTORY.csv")

colnames(airplanes_df)[2] <- "Tail_Number"

summary(airplanes_df) # ready

################################################################################
########### LET'S PREPARE THE DATASET OF CONCURRENT FLIGHTS ####################
################################################################################

df$Origin <- as.factor(df$Origin)
df$Dest <- as.factor(df$Dest)
df$DepTimeBlk <- as.factor(df$DepTimeBlk)
df$ArrTimeBlk <- as.factor(df$ArrTimeBlk)
df$FlightDate <- as.Date(df$FlightDate, "%Y-%m-%d")

concurrent_arrival <- df %>% group_by(FlightDate, Dest, ArrTimeBlk) %>% summarise(ConcurrentFlights = n())

concurrent_arrival <- subset(concurrent_arrival, Dest == "JFK" | Dest == "BOS" | Dest == "PHX")

#and then also the concurrent flights in departure: 

three_airports_df <- subset(df, Origin == "JFK" | Origin == "BOS" | Origin == "PHX")

rm(df) # It's too heavy and my computer is crying so I finally remove it. 
rm(weather_df) # also this one I dont need it anymore

three_airports_df <- droplevels(three_airports_df)

str(three_airports_df)
summary(three_airports_df)

#dropping some columns I will not use: 

three_airports_df <- three_airports_df %>% select(!c(CRSElapsedTime, Flight_Number_Marketing_Airline, DestCityName, CRSArrTime))

summary(three_airports_df)

concurrent_departure <- three_airports_df %>% group_by(Origin, FlightDate, DepTimeBlk) %>% summarise(ConcurrentFlights_Dep = n())


################################################################################
################ LAST THING TO DO IS MERGING EVERYTHING ########################
################################################################################

merge_weather <- merge(three_airports_df, weather_df_3, by = c("Origin", "FlightDate"))

merge_aircraft <- merge(merge_weather, airplanes_df, by = "Tail_Number")

colnames(concurrent_arrival)[2] <- "Origin"

merge_arrival <- merge(merge_aircraft, concurrent_arrival, by = c("FlightDate", "Origin", "ArrTimeBlk"))

colnames(merge_arrival)[21] <- "Concurrent_Arrival"

taxiTime_df <- merge(merge_arrival, concurrent_departure, by = c("FlightDate", "Origin", "DepTimeBlk"))

colnames(taxiTime_df)[22] <- "Concurrent_Departure"

################################################################################
############## NOW LET'S TAKE A LOOK AT THE DATASET ############################
################################################################################

str(taxiTime_df) #we dont need tail_number anymore

taxiTime_df <- taxiTime_df %>% select(!Tail_Number)

# I keep Airline as factor: 

taxiTime_df$Operating_Airline <- as.factor(taxiTime_df$Operating_Airline)

# I add Total Taxi: 

taxiTime_df$totalTaxi <- taxiTime_df$TaxiOut + taxiTime_df$TaxiIn

boxplot(totalTaxi ~ Origin, data = taxiTime_df) # there is some difference... maybe we can also drop BOS in the end but let's see. 

# I add the route_ID: 

taxiTime_df$routeID <- paste(taxiTime_df$Origin, taxiTime_df$Dest, taxiTime_df$Operating_Airline, sep = "-")

taxiTime_df$routeID <- as.factor(taxiTime_df$routeID)

# instead of manufacture year I want airplane age: 

taxiTime_df$age <- 2019 - taxiTime_df$MANUFACTURE_YEAR

taxiTime_df <- taxiTime_df %>% select(!MANUFACTURE_YEAR)

summary(taxiTime_df)

################################################################################
##################### A BIT OF CLEANING NOW ####################################
################################################################################

# we have so many data that we can cancel NAs

taxiTime_df <- na.omit(taxiTime_df)

summary(taxiTime_df)

# then, let's check which airlines operate in which airport (to only keep some of them): 

airlines_per_airport <- taxiTime_df %>% group_by(Origin, Operating_Airline) %>% summarise( total_flights = n())

# note: main ones operating in BOS are AA, B6, DL, YX, UA.
# main ones operating in JFK are 9E, AA, B6, DL, 
# main ones operating in PHX are AA, DL, WN, YV. 

# let's also check total taxi by airline: 

taxi_by_airline <- taxiTime_df %>% group_by(Operating_Airline) %>% summarise( avg_tot_taxi = mean(totalTaxi))

# note: 
# AA -> 28.21186 min; 
# DL -> 31.58570 min; 
# WN -> 16.78768 min; (because it only operates in PHX)
# 9E -> 34.15826 min; (because it only operates in JFK)

# let's try to keep only those ones: 

taxiTime_df <- taxiTime_df %>% filter( Operating_Airline == "AA" | Operating_Airline == "DL" | Operating_Airline == "WN" | 
                                         Operating_Airline == "9E") # we still have 250 K data, a lot :)

taxiTime_df <- droplevels(taxiTime_df)

boxplot(totalTaxi ~ Operating_Airline, data = taxiTime_df) # there is some difference again

summary(taxiTime_df)

# let's also eliminate those routes that do less than 1 flight per week: 

route_freq_df <- taxiTime_df %>% group_by(routeID) %>% summarise(route_freq = n())

final_taxiTime_df <- merge(taxiTime_df, route_freq_df, by = "routeID")

final_taxiTime_df <- final_taxiTime_df %>% subset(route_freq > 51) # still 250 K data so ok

final_taxiTime_df <- final_taxiTime_df %>% select(!c(route_freq))

final_taxiTime_df <- droplevels(final_taxiTime_df)

summary(final_taxiTime_df)

# what I'm thinking now is that we have too many data, and random forest and other algorithms take a lot of time in this way. 
# what I do is keeping only a more or less random subset of the data: 

# but first I add the month: 

library(lubridate)

final_taxiTime_df$month <- month(final_taxiTime_df$FlightDate)
final_taxiTime_df$month <- as.factor(final_taxiTime_df$month)
final_taxiTime_df$DayOfWeek <- as.factor(final_taxiTime_df$DayOfWeek)

summary(final_taxiTime_df)

# The idea now is that I only keep 20K random observations for each airport. Let's see what we obtain: 

df_BOS <- final_taxiTime_df %>% subset(Origin == "BOS")
df_JFK <- final_taxiTime_df %>% subset(Origin == "JFK")
df_PHX <- final_taxiTime_df %>% subset(Origin == "PHX")

sample1 <- sample_n(df_BOS, 20000)
sample2 <- sample_n(df_JFK, 20000)
sample3 <- sample_n(df_PHX, 20000)

sample_taxiTime <- rbind(sample1, sample2, sample3)

#write.csv(sample_taxiTime, "D:/ESAMI POLI/Applied Statistics/PROJECT/sample_taxiTime.csv")

# let's check how it looks: 

summary(sample_taxiTime) # looks ok 

table(sample_taxiTime$routeID) # now we have again some routes with less than 52 obs in a year, but for now it's ok. 