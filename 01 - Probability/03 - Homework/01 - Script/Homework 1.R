#GRADE = 52%

#PROBLEM 1
###########################

#A. Probability of 2 Dice < 7
probability_dice <- 0
(dice <- (1:6))

#For each dice number, 1 through 6
#Iterate through each possible combination
for (i in 1:6){
  for (n in 1:6){
    if (dice[i] + dice[n] < 7){
      probability_dice = probability_dice + 1
    }
  }
}

#Divide by total (6*6)
probability_dice = probability_dice / (36)
probability_dice

#The probability of two equally weighted dice 
#landing on two numbers that sum to less than 7


#B. Overall Probability of reaching 80
p70 <- 0.62
p80 <- 0.23

#If a person reaches their 70th birthday 
#what is the probability of reaching 80?
  #Conditional = p(B|A) = p(A,B)/p(A)
p7080 = (p80/p70)
p7080



#C. Checking and savings accounts
pchecking <-  0.62
psavings <- 0.24
pCS <- 0.17

#probability of having a savings account
#of the people having a checking account
psaving_check <- (pCS)/pchecking
psaving_check


#D. Probability that someone has 
#neither?!?
#p(neither) = p(1-savings) * p(1-check)
p_no_bank = (1-pchecking) * (1-pchecking) * (1-pCS)
p_no_bank

#E. Proportion of Iron Bank have a savings but
#not a checking
prop_savings <- psavings/(pchecking + psavings - pCS)
prop_savings



#PROBLEM 2 - plays_top50
##############################
library(tidyverse)
xtabs(~bob.dylan + the.beatles, data=plays_top50) %>% 
  prop.table() %>%
  round(3)




#PROBLEM 3 - Random Clicker
random_clickers <- 0.30
survey_yes <- 0.65
survey_no <- 0.35
#FIND p(yes|TC)

#1. Find p(TC|Yes)
#p(TC|Yes) = p(TC) * p(Yes|TC) / p(Yes)
truthful <- 1-random_clickers
pY_TC = (survey_yes * truthful)
pYES = (.50 * .65) + (0.30 * (1-.65))

pTCYES = (truthful * pY_TC)/pYES  
pTCYES 
  
  
  

#PROBLEM 4 - Texas SOS
sos_rate <- 1/1000
sos_population <- 10000000
sos_positive <- .95
sos_negative <- .99

#find p(SOS|positive)
# p(SOS|Positive) = p(SOS) * p(Positive|SOS) / p(Positive)
pSOS_pos = (sos_rate * sos_positive)/((sos_positive + (1-sos_negative)) * sos_rate)
pSOS_pos




#PROBELM 5 - s550
library(ggplot2)
library(scales)
options(scipen=999)
ggplot(s550) +
  geom_point(aes(x=mileage, y=price, color=price)) +
  labs(title = "1501 Mercedes S-Class S550 Cars", 
       x = "Mileage",
       y = "Price")+
  facet_wrap(~year)




#PROBLEM 6 - bikeshare
hour_and_time <- subset(bikeshare[, c("hr", "total", "workingday", "weathersit")])
hour_and_time
agg <- aggregate(hour_and_time,
                 by = list(hour_and_time$hr),
                 FUN = mean)
agg
  
#PLOT 1
ggplot(agg) + 
  geom_line(aes(x=hr, y=total))


#PLOT 2
working_day = filter(
  hour_and_time, workingday == 1
)
off_day = filter(
  hour_and_time, workingday == 0
)

aggreaged_working = aggregate(working_day,
                              by = list(working_day$hr),
                              FUN = mean) 
aggreaged_off = aggregate(off_day,
                              by = list(off_day$hr),
                              FUN = mean) 
par(mfrow=c(1,2))
ggplot(aggreaged_working) +
  geom_line(aes(x=hr, y=total))
ggplot(aggreaged_off) +
  geom_line(aes(x=hr, y=total))
  

#PLOT 3
past_8am = filter(hour_and_time, hr==8)
head(past_8am)

on_8 = filter(past_8am, workingday == 1)
off_8 = filter(past_8am, workingday == 0)

head(off_8)
a_on = aggregate(on_8, 
                 by = list(on_8$weathersit),
                 FUN = mean)
a_off = aggregate(off_8, 
                 by = list(off_8$weathersit),
                 FUN = mean)

par(c(1,2))
  ggplot(a_on) +
    geom_line(aes(x=weathersit, y=total))
  ggplot(a_off) +
    geom_line(aes(x=weathersit, y=total))

