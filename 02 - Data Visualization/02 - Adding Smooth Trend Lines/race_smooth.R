cat("\f")

library(tidyverse)
library(ggplot2)
library(mosaic)

data(TenMileRace)
head(TenMileRace)

# our basic scatterplot
# (saved as p0 for subsequent layering)
p0 = ggplot(data=TenMileRace) + 
  geom_point(aes(x=age, y=net), alpha=0.1)
p0

# Adding a smoothing layer:
# see geom_smooth for the gory details! (very, very optional)
p0 + geom_smooth(aes(x=age, y=net))


# manually change the color of the trend line
p0 + geom_smooth(aes(x=age, y=net), color='limegreen')

# if you want to suppress the error bands
p0 + geom_smooth(aes(x=age, y=net), color='limegreen', se=FALSE)

# If multiple layers share the same aesthetic mapping,
# you might find it more concise to put the mapping in
# the ggplot command itself
ggplot(data=TenMileRace, mapping=aes(x=age, y=net)) + 
  geom_point(alpha=0.1) + 
  geom_smooth()

# geom_smooth also plays nicely with other ggplot layers
p0 +  facet_wrap(~sex) + 
  geom_smooth(aes(x=age, y=net), color='limegreen', se=FALSE)

# A facet grid
local_states = c('DC', 'VA', 'MD')
TenMileRace = TenMileRace %>%
  mutate(local = ifelse(state %in% local_states, 'Local', 'Out of State'))

head(TenMileRace)


ggplot(data=TenMileRace) + 
  geom_point(aes(x=age, y=net), alpha=0.1) +
  facet_grid(sex ~ local) + 
  geom_smooth(aes(x=age, y=net), color='limegreen')
