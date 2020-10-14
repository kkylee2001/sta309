#Homework 3
#Kyle Look Fong
#STA309
################################################
#Library Import
library(tidyverse)
library(mosaic)
library(dplyr)
library(ggplot2)

times <- 10000



################################################
#Problem 1
################################################
#Question: Create a model to assess support for any male/female differences 
#in the population-wide rate of 'left arm on top folding'
arm_gender <- do(times)*lm(LonR_fold ~ Sex, data=resample(armfold))

ggplot(arm_gender)+
  geom_histogram(aes(x=SexMale))

confint(arm_gender, level = 0.95)%>%
  mutate_if(is.numeric, round, digits=3)





################################################
#Problem 2
################################################
#Question: Which theories seem true? 

#A. Gas stations charge more if they lack direct competition in sight ~ Create a dummy variable 
GasPrices <- GasPrices %>%
  mutate(Competitors_dummy = ifelse(Competitors == 'Y', 1, 0))


#####i. Linear Regression without bootstrapping, to see the differences
(lm(Price ~ Competitors_dummy, data = GasPrices))

#####ii. Bootstrap the data with replacement to make the sample more representative, then create a regression model
lm_competition <- do(times)*lm(Price ~ Competitors_dummy, data = resample(GasPrices))

ggplot(lm_competition)+
  geom_histogram(aes(x=Competitors_dummy), binwidth = 0.001)+
  geom_vline(xintercept = -0.02348, color='red')


#####iii. Determine the confidence intervals and the margin of error for the experiment
confint(lm_competition)%>%
  mutate_if(is.numeric, round, digits=3)

#####iv. Conclusion
#It is  plausible that this is supported by the data due to the fact that with a bootstrapped model, gas stations with competitors paid 0.23 less on average

#####Visualization of Example

ggplot(GasPrices)+
  geom_boxplot(aes(x=Competitors, y=Price))



################################################
## The steps are the same from above ##
#B. The richer the area, the higher the gas price

(lm(Price~Income, data = GasPrices))

lm_income <- do(times)*lm(Price~Income, data = resample(GasPrices))

ggplot(lm_income)+
  geom_histogram(aes(x=Income))

confint(lm_income)%>%
  mutate_if(is.numeric, round, digits=9)


################################################

#C. Gas stations at stoplights charge more
GasPrices <- GasPrices %>%
  mutate(Stoplight_Dummy = ifelse(Stoplight == 'Y', 1, 0))


lm_light <- do(times)*lm(Price ~ Stoplight_Dummy, data = resample(GasPrices))

ggplot(lm_light)+
  geom_histogram(aes(x=Stoplight_Dummy), binwidth = 0.003)


confint(lm_light)%>%
  mutate_if(is.numeric, round, digits=3)

ggplot(GasPrices)+
  geom_boxplot(aes(y=Price, group=1))+
  facet_wrap(~Stoplight_Dummy)

ggplot(GasPrices)+
  geom_boxplot(aes(x=Stoplight, y=Price))



################################################

#D. Gas stations with direct highway access charge more
GasPrices <- GasPrices %>%
  mutate(Highway_Dummy = ifelse(Highway == 'Y', 1, 0))


lm_highway <- do(times)*lm(Price ~ Highway_Dummy, data = resample(GasPrices))


confint(lm_highway)%>%
  mutate_if(is.numeric, round, digits=3)



ggplot(GasPrices)+
  geom_boxplot(aes(x=Highway, y=Price))


################################################

#E. Shell charges more than all non-Shell brands
GasPrices <- GasPrices %>%
  mutate(Shell_Dummy = ifelse(Name == 'Shell', 1, 0))

lm_shell <- do(times)*lm(Price ~ Shell_Dummy, data = resample(GasPrices))

confint(lm_shell)%>%
  mutate_if(is.numeric, round, digits=3)



ggplot(GasPrices)+
  geom_boxplot(aes(x=Name, y=Price))



################################################
#Problem 3
################################################
#Question: Does the extra traffic brought to ebay's site through
#sponsored search results justify the cost? 
##FIT A MODEL AND USE A RANDOMIZATION TEST to assess whether the 
#REVENUE RATIO is the same in the treatment and control groups, or 
#whether instead the data favors that paid search advertising creates
#extra revenue
#at least 10,000 <- times


#NOTE: PAUSE = treatment of NO ADS
(lm_ads <- lm(rev_ratio ~ adwords_pause, data = ebay))
#If the ads were paused, they experienced ~ 0.05228 decline in the ratio


randomization_ebay <- do(times) * lm(rev_ratio ~ shuffle(adwords_pause), data = ebay)

ad_pause_graph <- geom_vline(xintercept=-0.05228, color='red')

ggplot(randomization_ebay)+
  geom_histogram(aes(x=adwords_pause), binwidth = 0.001)+
  ad_pause_graph

sum(randomization_ebay$adwords_pause <= -0.05228)/times
#0.006, P value < 1%. 
#P value concludes that the 0.05228 decline in distribution is not by chance

