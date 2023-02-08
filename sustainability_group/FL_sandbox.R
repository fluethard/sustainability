library(tidyverse)      # select
library(ggplot2)    # ggplot
library(reshape2) 

air_co <- air %>%
  select(Datum, CO)

plot(air_co)

# trees by year
tree_ts <- d.trees %>%
  select(pflanzjahr, objid) %>%
  filter(pflanzjahr > 1980) %>%
  group_by(pflanzjahr) %>%
  summarise(count = n())

# Trees by district
tree_quartier <- d.trees %>%
  select(pflanzjahr, objid, quartier) %>%
  filter(pflanzjahr > 1980) %>%
  group_by(pflanzjahr, quartier) %>%
  summarise(count = n())


#summarise CO2 by year
yearly_air <- air %>%
  add_column(year = lubridate::year(air$Datum))

yearly_co <- yearly_air %>%
  select(year, CO) %>%
  group_by(year) %>%
  summarise(mean_co = mean(CO, na.rm = T))

# sum of trees
tree_ts_sum <- tree_ts %>%
 mutate(sum_trees = cumsum(count))




