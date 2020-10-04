library(tidyverse)
library(ggplot2)
library(mosaic)

# a scatterplot
ggplot(heartrate) + 
  geom_point(aes(x=age, y=hrmax)) 

# fit the model
lm1 = lm(hrmax ~ age, data=heartrate)

# summarize the trend
coef(lm1)

# calculate r squared for the model
rsquared(lm1)

# note: rsquared is also at the bottom of the "regression table":
summary(lm1)

# make a prediction
# Let's read in a new data frame with ages for people
# whose hrmax we want to predict

# here are our model's predictions
# the predict function expects a data frame whose column names
# match the x variable (or variables) in the original data set
predict(lm1, newdata = heartrate_test)

# let's add these predictions to the heartrate_test data frame
heartrate_test = heartrate_test %>%
  mutate(hrmax_pred = predict(lm1, newdata = heartrate_test))

# we don't know their actual hrmax, but we do have our model predictions
heartrate_test


# finally, let's do "statistical adjustment."
# Remember: the model residuals represent the difference between
# actual hr_max and predicted/fitted hr_max.
# in this case: hrmax, adjusted for age.
fitted(lm1) # the model fit for each data point
resid(lm1) # the model residual for each data point

# it's a little more helpful if we put these side by side
# with the original data points
heartrate_aug = heartrate %>%
  mutate(hrmax_pred = fitted(lm1), 
         hrmax_resid = resid(lm1))

heartrate_aug


# we can even arrange by the residuals: in ascending order....
heartrate_aug %>%
  arrange(hrmax_resid) %>%
  head(10)

# or in descending order
heartrate_aug %>%
  arrange(desc(hrmax_resid)) %>%
  head(10)
