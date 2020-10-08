library(tidyverse)
library(mosaic)
library(ggplot2)
library(dplyr)


######################################
#PROBLEM 2 - Creatinine 
######################################

#Plotting the clearance on a scatterplot 
clearance <- ggplot(creatinine)+
  geom_point(aes(x=age, y=creatclear))+
  labs(title = "Creatinine Clearance by Age")+
  ylab("Creatinine Clearance (ml/min)")+
  xlab("Age")

#Creating the linear model
(lm_clearance <- lm(creatclear ~ age, data = creatinine))

#finding the coefficients/summarizing the trend
coef(lm_clearance)

#Regression table
summary(lm_clearance)

#Making a predictions table for each age 1-100
predictions <- data.frame(age=c(1:100))
predictions <- predictions %>%
  mutate(pred_clearance = predict(lm_clearance, newdata = predictions))

#Making a predictions table for the given ages and given rates
age_pred <- data.frame(age=c(40,60), creatclear=c(135,112))
age_pred <- age_pred %>%
  mutate(prediction = predict(lm_clearance, newdata = age_pred))



######################################
#PROBLEM 3 - Epidemiologists
######################################

#Setting the constants, the number of people and the COPD rate
(N <- 1072)
(COPD_Rate <- 0.033)

#Doing the calculation 100,000 times and setting it to a variable
COPD_distribution <- do(100000)*nflip(n = N, prob = COPD_Rate)
head(COPD_distribution)

#Plotting observations with ggplot2
ggplot(COPD_distribution)+
  geom_histogram(aes(x=nflip, fill = nflip < 42) ,binwidth = 0.33)+
  labs(title = "Probability Distribution of COPD (rate: 3.3%)")+
  xlab("Number of COPD Cases") +
  ylab("Count out of 100,000")+
  geom_vline(xintercept = 42, color = "red")

#Finding the p-value
(COPD_p_value <- sum(COPD_distribution >=42)/100000)



######################################
#PROBLEM 4 - Market Model
######################################

#Creating individual tables
copy_ticker <- function(ticker_name){
  select(marketmodel, Date, SPY, ticker_name)
}
GOOG_df <- copy_ticker("GOOG")
AAPL_df <- copy_ticker("AAPL")
MRK_df <- copy_ticker("MRK")
JNJ_df <- copy_ticker("JNJ")
WMT_df <- copy_ticker("WMT")
TGT_df <- copy_ticker("TGT")

#Making regression for each
(lm_GOOG <- lm(SPY~GOOG, data=GOOG_df))
(lm_AAPL <- lm(SPY~AAPL, data=AAPL_df))
(lm_MRK <- lm(SPY~MRK, data=MRK_df))
(lm_JNJ <- lm(SPY~JNJ, data=JNJ_df))
(lm_WMT <- lm(SPY~WMT, data=WMT_df))
(lm_TGT <- lm(SPY~TGT, data=TGT_df))

r2_GOOG <- round(rsquared(lm_GOOG), 3)
r2_AAPL <- round(rsquared(lm_AAPL), 3)
r2_MRK <- round(rsquared(lm_MRK), 3)
r2_JNJ <- round(rsquared(lm_JNJ), 3)
r2_WMT <- round(rsquared(lm_WMT), 3)
r2_TGT <- round(rsquared(lm_TGT), 3)

#Adding prediction to table
make_prediction <- function(df, lm){
  df <- df %>%
    mutate(prediction = round(predict(lm, newdata = df), 4))
}

GOOG_df <- make_prediction(GOOG_df, lm_GOOG)
AAPl_df <- make_prediction(AAPL_df, lm_AAPL)
MRK_df <- make_prediction(MRK_df, lm_MRK)
JNJ_df <- make_prediction(JNJ_df, lm_JNJ)
WMT_df <- make_prediction(WMT_df, lm_WMT)
TGT_df <- make_prediction(TGT_df, lm_TGT)

#Making a table with all the attributes
(beta <- data.frame(ticker = c("GOOG", "AAPL", "MRK", "JNJ", "WMT", "TGT"),
                   intercept = c(
                              round(as.numeric(lm_GOOG$coefficients[1]),3),
                              round(as.numeric(lm_AAPL$coefficients[1]),3),
                              round(as.numeric(lm_MRK$coefficients[1]),3),
                              round(as.numeric(lm_JNJ$coefficients[1]),3),
                              round(as.numeric(lm_WMT$coefficients[1]),3),
                              round(as.numeric(lm_TGT$coefficients[1]),3)),
                   rate = c(
                            round(as.numeric(lm_GOOG$coefficients[2]),3),
                            round(as.numeric(lm_AAPL$coefficients[2]),3),
                            round(as.numeric(lm_MRK$coefficients[2]),3),
                            round(as.numeric(lm_JNJ$coefficients[2]),3),
                            round(as.numeric(lm_WMT$coefficients[2]),3),
                            round(as.numeric(lm_TGT$coefficients[2]),3)),
                   Rsq = c(r2_GOOG, r2_AAPL, r2_MRK, r2_JNJ, r2_WMT, r2_TGT),
                   market_up = c(
                     round((as.numeric(lm_GOOG$coefficients[1]) + as.numeric(lm_GOOG$coefficients[2]) * 0.02),4),
                     round((as.numeric(lm_AAPL$coefficients[1]) + as.numeric(lm_AAPL$coefficients[2]) * 0.02),4),
                     round((as.numeric(lm_MRK$coefficients[1]) + as.numeric(lm_MRK$coefficients[2]) * 0.02),4),
                     round((as.numeric(lm_JNJ$coefficients[1]) + as.numeric(lm_JNJ$coefficients[2]) * 0.02),4),
                     round((as.numeric(lm_WMT$coefficients[1]) + as.numeric(lm_WMT$coefficients[2]) * 0.02),4),
                     round((as.numeric(lm_TGT$coefficients[1]) + as.numeric(lm_TGT$coefficients[2]) * 0.02),3)
                   ),
                   market_down = c(
                     round((as.numeric(lm_GOOG$coefficients[1]) + as.numeric(lm_GOOG$coefficients[2]) * -0.02),4),
                     round((as.numeric(lm_AAPL$coefficients[1]) + as.numeric(lm_AAPL$coefficients[2]) * -0.02),4),
                     round((as.numeric(lm_MRK$coefficients[1]) + as.numeric(lm_MRK$coefficients[2]) * -0.02),4),
                     round((as.numeric(lm_JNJ$coefficients[1]) + as.numeric(lm_JNJ$coefficients[2]) * -0.02),4),
                     round((as.numeric(lm_WMT$coefficients[1]) + as.numeric(lm_WMT$coefficients[2]) * -0.02),4),
                     round((as.numeric(lm_TGT$coefficients[1]) + as.numeric(lm_TGT$coefficients[2]) * -0.02),4)
                     
                   )))
