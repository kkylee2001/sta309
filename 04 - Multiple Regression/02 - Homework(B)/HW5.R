#Homework 5

#Library Import
############################
require(mosaic)
require(ggplot2)
require(tidyverse)
require(MatchIt)
source('https://tinyurl.com/y4krd9uy') #ANOVA functions

#From HW Notes
capmetro_UT = mutate(capmetro_UT,
                     day_of_week = factor(day_of_week,
                                          levels=c("Mon", "Tue", "Wed","Thu", "Fri", "Sat", "Sun")),
                     month = factor(month,
                                    levels=c("Sep", "Oct","Nov")))
############################
#1. Campetro_UT
############################
#Plot 1 - line graph
#plots average boardings grouped by hour, day of week, and month
#facet by day of week
#three lines: one for each month, colored differently and labeled
#in a legend

pt1 <- capmetro_UT %>%
  group_by(hour_of_day, day_of_week, month) %>%
  summarize(boardings = mean(boarding))

ggplot(pt1)+
  geom_line(aes(x=hour_of_day, y=boardings, group=month, color=month))+
  facet_wrap(~day_of_week)+
  ggtitle("Average Boardings")+
  ylab("Boardings")+
  xlab("Hour")
#ANSWER QUESTIONS IN PDF
#======================================

#Plot 2 - scatter plot
#boarding (y) v. temperature (x) in each
#15 minute window, faceted by hour of day
#with points colored in according to
#weekday or weekend

ggplot(capmetro_UT)+
  geom_point(aes(x=temperature, y=boarding, group=timestamp, color=weekend))+
  facet_wrap(~hour_of_day)+
  xlab("Temperature ºF")+
  ylab("Boarding")+
  ggtitle("Boarding by Temperature Every Hour")


############################
#2. Green Hack
############################
hack_A <- do(1000)*{
lm(rev_psf ~ green_rating + age + class + City_Market_Rent, data=resample(green_hack))
}

model <- data.frame(confint(hack_A)) #To easily populate excel

hack_B <- do(1000)*{
lm(rev_psf ~ green_rating + age + class + City_Market_Rent + 
     class:City_Market_Rent, 
   data = resample(green_hack))
}

model <- data.frame(confint(hack_B))


############################
#3. Covid
############################
#Line Plot: Using the days as the x and deaths on the y
ggplot(covid, aes(x=days_since_first_death, y=deaths))+
  geom_line()+
  facet_wrap(~country)+
  ylab("Deaths")+
  xlab("Days since first Death")+
  ggtitle("COVID-19 Deaths by Days from First Death")

#Line Plot: Using the days as the x and deaths on the y
ggplot(covid, aes(x=days_since_first_death, y=deaths, group=country, color=country))+
  geom_line() #For my refernece, easier to see for me


#Creating separate data frames for each country
italy <- covid %>%
  filter(country == 'Italy')

spain <- covid %>%
  filter(country == 'Spain')


#Creating an exponential growth model
#Italy
#---------------------------------
lm_italy <- do(1000)*{
  lm(log(deaths) ~ days_since_first_death, 
                 data = resample(italy))
  }
model <- confint(lm_italy)
model <- model %>%
  mutate(low_double = log(2)/lower,
         up_double = log(2)/upper,
         est_double = log(2)/estimate)

#Spain
#---------------------------------
lm_spain <- do(1000)*{
  lm(log(deaths) ~ days_since_first_death, 
     data = resample(spain))
}
model <- confint(lm_spain)
model <- model %>%
  mutate(low_double = log(2)/lower,
         up_double = log(2)/upper,
         est_double = log(2)/estimate)




