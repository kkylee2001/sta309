library(mosaic)
library(tidyverse)

# Remember we can use nflip to simulate binary outcomes, like this:
nflip(n=25, prob=0.5)

# Now we'll try 80,515 flips of a metaphorical biased "coin"
# that comes up heads 4.7 times per 10,000 flips (prob = 0.00047), on average.
# This is the base rate of leukemia among Illinois children
# living more than 30 miles from a nuclear plant.
# In our metaphor, each occurrence of "heads" corresponds to a case of cancer.
nflip(n=80515, prob=0.00047)

# repeat this simulation 10000 times and store the result
sim_cancer = do(10000)*nflip(n=80515, prob=0.00047)

# As before, the result is a data frame with one column called "nflip"
head(sim_cancer)

# Visualize the distribution
ggplot(sim_cancer) + 
  geom_histogram(aes(x=nflip), binwidth=0.5)

# How many simulations yield 47 cases of cancer or more, just by chance?
sum(sim_cancer >= 47)

# As a proportion of the total number of simulations?
sum(sim_cancer >= 47)/10000

# Optional:
# a much faster shortcut using the binomial distribution
# the binomial distribution is the probability distribution
# that describes the results of flipping "biased" coins with
# a given "heads" probability.
sim_cancer_2 = rbinom(n=10000, size = 80515, prob = 0.00047)
sum(sim_cancer_2 >= 47)/10000

# upshot: this second code snippet runs much faster and gives
# the same answer, but is a but harder to understand what's
# happening "under the hood"
