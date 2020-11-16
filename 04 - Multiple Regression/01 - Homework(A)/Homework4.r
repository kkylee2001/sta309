#Kyle Look Fong
#STA 309
#Homework 4
###########################################

#Packages
require(mosaic)
require(ggplot2)
require(tidyverse)
source('https://tinyurl.com/y4krd9uy') #ANOVA functions
#NOTE: model = to copt/paste into excel

###########################################




#1. 
###########################################
#Build a linear model for peak power consumption
#that includes main effects for temperature and weekday
#as well as an interaction between temperature
#and workday

    #Build linear model
(lm(COAST ~ temp + weekday + temp:weekday, data = load_summer))
lmPower <- do(10000)*lm(COAST ~ temp + weekday + temp:weekday, data = resample(load_summer))

    #Determine confidence intervals
confint(lmPower)

    #Fit on anova table
simple_anova(lm(COAST ~ temp + weekday + temp:weekday, data = load_summer))

#--------------------

    #Plot a faceted scatter plot
ggplot(load_summer)+
  geom_jitter(aes(x=temp, y=COAST))+
  facet_wrap(~weekday)+
  ggtitle("Peak Power Consumption by Temperature and Weekend Status")+
  xlab("Temperature (ºC)")+
  ylab("Peak Power Consumption (M-Watts)")
###########################################




#2. 
###########################################
#Build a linear regression model for the rent of a two
#bedroom apt in wampus in terns of the other variables
#twobed_sf, distance_from_tower, furnished, pool, laundry_in_unit, 
#electricity, and water
    #Build a linear model
lmApartments <- do(10000)*(lm(twobed_rent ~ twobed_sf + distance_from_tower +
    furnished + pool + laundry_in_unit + electricity + water, 
    data=resample(apartments)))

    #Determine Confidence intervals
model <- data.frame(confint(na.omit(lmApartments)))

#--------------------
    #Run un-adjusted model -- distance from tower
lmAptTower <- do(10000)*lm(twobed_rent ~ distance_from_tower, 
                           data = resample(apartments))

model <- data.frame(confint(na.omit(lmAptTower)))

#--------------------
    #Run an un-adjusted model, only on twobed_sf
lmAptSQFT <- do(10000)*lm(twobed_rent ~ twobed_sf, 
                           data = resample(apartments))

model <- data.frame(confint(na.omit(lmAptSQFT)))

#--------------------
    #Scatter Plot 1: rent v. distance from tower
ggplot(apartments)+
  geom_jitter(aes(x=distance_from_tower, y=twobed_rent))+
  ylim(600,1500)+
  ggtitle("Rent by Distance to Tower")+
  xlab("Distance From Tower (Miles)")+
  ylab("Rent ($)")

    #Scatter Plot 2: Rent v. Sqft
ggplot(apartments)+
  geom_jitter(aes(x=twobed_sf, y=twobed_rent))+
  ylim(600,1500)+
  ggtitle("Rent by SQFT")+
  xlab("Square Feet (ft²)")+
  ylab("Rent ($)")
###########################################




#3. 
###########################################
#Use a linear regression model to access whether there is 
#an association between the number of fair policies and the 
#racial/ethnic composition of a zip code
#adjusting for the fire, age, and income variables

#NOTES: policies = per 100, minorities = percentage of residents
#fires is per 100, age = percent before WWII
#income in thousands of dollars. MORE POLICIES implies less access

#Run a linear model without accounting for race, bootstrap
lmNoRace <- do(10000) * lm(policies ~ income + age + fire,
                           data = resample(redline))

#Make a linear regression model to predict the policies
lmNoRaceA <- lm(policies ~ income + age + fire,
                data = resample(redline))

#Add these predictions to the data table
redline <- redline %>%
  mutate(NRprediction = predict(lmNoRaceA, newdata=redline))

#Determine Confidence intervals
model <- data.frame(confint(na.omit(lmNoRace)))


#--------------------
#Create a linear regression model with bootstrap
lmRace <- do(10000) * lm(policies ~ minority + income + age + fire,
                         data = resample(redline))

#Create linear regression model to predict policies
lmRaceA <- lm(policies ~ minority + income + age + fire,
              data = resample(redline))

#Add prediction to predict the policies
redline <- redline %>%
  mutate(Rprediction = predict(lmRaceA, newdata=redline))

#Determine Confidence Intervals
model <- confint(na.omit(lmRace))

#--------------------
#Create a linear regression model with bootstrap
lmZIP <- do(10000) * lm(policies ~ ZIP + income + age + fire,
                         data = resample(redline))

#Determine Confidence Intervals
model <- confint(na.omit(lmZIP))


#--------------------
lmOnlyRace <- do(10000)*lm(policies ~ minority, data=resample(redline))

#Create linear regression model to predict policies
lmOnlyRaceA <- lm(policies ~ minority,
              data = resample(redline))

#Add prediction to predict the policies
redline <- redline %>%
  mutate(OnlyRprediction = predict(lmOnlyRaceA, newdata=redline))

#Determine Confidence Intervals
model <- confint(na.omit(lmOnlyRace))


#Put on ANOVA Table
simple_anova(lmNoRaceA)
simple_anova(lmRaceA)


#--------------------
#Create a scatter plot, one adjusting for race
#one not adjusting for race
ggplot(redline)+
  geom_jitter(aes(x=minority, y=NRprediction))+
  ggtitle("Predicted FAIR Policies by Minority, Unadjusted")+
  xlab("Minority (%)")+
  ylab("Predicted FAIR Policies")

ggplot(redline)+
  geom_jitter(aes(x=minority, y=Rprediction))+
  ggtitle("Predicted FAIR Policies by Minority, Adjusted")+
  xlab("Minority (%)")+
  ylab("Predicted FAIR Policies")



ggplot(redline)+
  geom_jitter(aes(x=minority, y=OnlyRprediction))+
  ggtitle("Predicted FAIR Policies by Minority, Only Adjusted by Race")+
  xlab("Minority (%)")+
  ylab("Predicted FAIR Policies")
