#Kyle Look Fong
#STA 309 HW 6

#--------------------Librarys--------------------#
require(tidyverse)
require(ggplot2)
require(mosaic)
require(lubridate)
require(modelr)
require(rsample)
require(dplyr)



#-----------------------1------------------------#
#Build a model for sales in terms of price,
#the store level dummy variables, and a dummy
#variable for whether or not there was a display


#1. Creating a linear model, with the intervals
lm_cheese <- do(1000)*{
  (lm(log(vol) ~ log(price) + disp + price:disp, 
      data=mosaic::resample(cheese)))
}
model <- confint(lm_cheese)
model

#####FROM MILK.R
#3. Determining the optimal price
#optimizing gross profit, on non-display weeks
boot_price = do(1000)*{
  # resample the data and fit the model
  lm_boot = lm(log(vol) ~ log(price) + disp + price:disp, 
               data=mosaic::resample(cheese))
  beta_hat = coef(na.omit(lm_boot))
  
  # extract the coefficients of our fitted model
  logK = beta_hat[1]
  elasticity = beta_hat[2]
  
  # define our profit function
  profit = function(P, unit_cost=1) {
    exp(logK) * P^(elasticity) * (P - unit_cost)
  }
  
  # optimize the profit function
  optimal_solution = optimize(profit, lower=0, upper = 10, maximum=TRUE)
  
  #  return the value of price at the optimum
  optimal_solution$maximum
}

model <- confint(boot_price)


#-----------------------2------------------------#
#Compare the out of sample performance of four models
hotels_train <- hotels_train %>%
  mutate(arrival_date = ymd(arrival_date))

hotels_test <- hotels_test %>%
  mutate(arrival_date = ymd(arrival_date))



##################1. A small model that uses only the
#market segment, adults, customer_type, and is_repeated_guest
hotel_small <- lm(children ~ market_segment + adults + 
                    children + customer_type + is_repeated_guest,
                  data=hotels_train)


#In Sample
in_small <- rmse(hotel_small, hotels_train)

#Out of Sample
out_small <- rmse(hotel_small, hotels_test)




##################2. a big model that uses all except for arrival_date
hotel_big <- lm(children ~ . -arrival_date, data=hotels_train)


#In Sample
in_big <- rmse(hotel_big, hotels_train)


#Out of Sample
out_big <- rmse(hotel_big, hotels_test)



##################3. A huge model that uses all predictors and all possible 
#pairwise interactions
hotel_huge <-  lm(children ~ (. -arrival_date)^2, data=hotels_train)


#In Sample
in_huge <- rmse(hotel_huge, training)


#Out of Sample
out_huge <- rmse(hotel_huge, hotels_test)

##################4. Model 2 with the month of the year
training <- hotels_train %>% 
  mutate(month = month(arrival_date) %>% factor())

hotel_date <- lm(children ~ . -arrival_date, data=training)

testing <- hotels_test %>%
  mutate(month = month(arrival_date) %>% factor())
#In Sample
in_date <- rmse(hotel_date, training)

#Out of Sample
out_date <- rmse(hotel_date, testing)



model <- data.frame(
  training = c(in_small, in_big, in_huge, in_date),
  testing = c(out_small, out_big, out_huge, out_date)
)



#-----------------------3------------------------#
#Altering the dataset
sum_away <- epl_2018.19_away %>% summarise_each(funs(mean))
sum_away <- sum_away$GF + sum_away$GA
sum_away
epl_2018.19_away <- epl_2018.19_away %>% mutate(
  attack_strength = GF/sum_away,
  defense_weakness = GA/sum_away
)

sum_home <- epl_2018.19_home %>% summarise_each(funs(mean))
sum_home <- sum_home$GF + sum_home$GA
sum_home
epl_2018.19_home <- epl_2018.19_home %>% mutate(
  attack_strength = GF/sum_home,
  defense_weakness = GA/sum_home
)

leage_calc <- function(home, away, desire_table=FALSE){
  #19 teams that played for 19 times
  total_games <- 19*19 
  
  total_home <- rowSums(epl_2018.19_home[7])
  sum(total_home)
  expected_home <- total_games / total_home
  
  total_away <- rowSums(epl_2018.19_away[7])
  sum(total_away)
  expected_away <- total_games / total_away
  
  #Getting the appropriate data frames for each
  t1_away <- epl_2018.19_away %>%
    filter(Team == home)
  
  t2_away <- epl_2018.19_away %>%
    filter(Team == away)
  
  t1_home <- epl_2018.19_home %>%
    filter(Team == home)
  
  t2_home <- epl_2018.19_home %>%
    filter(Team == away)
  
  
  #Getting the average scores for the team away
  lambda_home <- t1_home$attack_strength * t2_away$defense_weakness * expected_home
  lambda_away <- t2_away$attack_strength * t1_home$defense_weakness * expected_away
 
  NMC = 100000
  t1 = rpois(NMC, lambda_home)
  t2 = rpois(NMC, lambda_away)
  
  # Compile the results
  xtabs(~t1 + t2)
  results_table = xtabs(~t1 + t2)/NMC
  results_table
  
  # Monte Carlo estimates of probabilities
  team1 <- sum(t1 > t2)/NMC
  tie <- sum(t1 == t2)/NMC
  team2 <- sum(t1 < t2)/NMC
  
  df <- data.frame(
    home = team1, tied = tie, away = team2
  )
  
  if(desire_table){
    return(results_table)
  }else{
  return(df)
  }
}

model <- leage_calc(home="Liverpool", away="Tottenham")
model <- leage_calc(home="Manchester City", away="Arsenal")

#-----------------------4------------------------#
#Risk is 0.10, after round of betting, 
#risk the same fraction. If you win, you place the new
#wealth, then continue

take_wager <- function(p, mc_sim, init_wealth, c, ylim){
  #Setting up the models
  model <- data.frame()
  for(i in 1:4){
    wealth <- init_wealth
    for (j in 1:mc_sim){
      this_bet <- rbinom(1, size=1, prob=p)
      winning_amount <- wealth * c
      if (this_bet == 0){
        winning_amount = winning_amount * -1
      }
      wealth <- wealth + winning_amount
      df <- data.frame(run = j, wealth=wealth, sim=i)
      model <- rbind(model, df)
    }
  }
  graph <- ggplot(model) +
    geom_line(aes(x=run, y=wealth))+
    ylim(0,ylim)+
    xlab("Run Times")+
    ylab("Wealth $")+
    ggtitle("Wealth Simulations")+
    facet_wrap(~sim)
  return (graph)
}


##### PART A #####
p <- 0.52
c <- 0.10
init_wealth <- 10000
sims <- 10000
ylim = 100000
t1 <- take_wager(p, sims, init_wealth, c, ylim)
t1

##### PART B #####
p <- 0.52
c <- 0.05
init_wealth <- 10000
sims <- 10000
ylim = 1000000
t2 <- take_wager(p, sims, init_wealth, c, ylim)
t2



##### PART C #####
p <- 0.52
c <- 0.02
init_wealth <- 10000
sims <- 10000
ylim = 1000000
t3 <- take_wager(p, sims, init_wealth, c, ylim)
t3+
  xlab("Run Time")+
  ylab("Wealth $")

