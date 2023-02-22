 # Dataset generation for dashboard


library(tidyverse)      # select
library(ggplot2)    # ggplot
library(reshape2) 

air_co <- air %>%
  dplyr::select(Datum, CO)

plot(air_co)

# trees by year
tree_ts <- d.trees %>%
  dplyr::select(pflanzjahr) %>%
  filter(pflanzjahr > 1980) %>%
  group_by(pflanzjahr) %>%
  summarise(count = n())

# Trees by district
tree_quartier <- d.trees %>%
  dplyr::select(pflanzjahr, objid, quartier) %>%
  filter(pflanzjahr > 1980) %>%
  group_by(pflanzjahr, quartier) %>%
  summarise(count = n())



# sum of trees
tree_ts_sum <- tree_ts %>%
  mutate(sum_trees = cumsum(count))


# Trees by district, year
tree_quartier_year_full <- d.trees %>%
  dplyr::select(pflanzjahr, quartier, kronendurchmesser) %>%
  filter(pflanzjahr > 1980) %>%
  group_by(pflanzjahr, quartier) %>%
  summarise(count = n(), sum_crown = sum(kronendurchmesser))

# trees by year
tree_year <- d.trees %>%
  dplyr::select(pflanzjahr, kronendurchmesser) %>%
  filter(pflanzjahr > 1980) %>%
  group_by(pflanzjahr) %>%
  summarise(tree_count = n(), crown_sum = sum(kronendurchmesser))

tree_year_sum <- tree_year %>%
  mutate(cum_trees = cumsum(tree_count)) %>%
  mutate(cum_crown = cumsum(crown_sum))


plot(tree_ts)

# TO DO
# Stitch Trees + Air + Temp
# Lora Sensor Data
# Tableau

tree_year_sum %>%
  ggplot(mapping = aes(x = pflanzjahr, y = cum_crown)) + geom_line()


tree_year_quartier <- d.trees %>%
  dplyr::select(pflanzjahr, quartier, kronendurchmesser) %>%
  filter(pflanzjahr > 1980) %>%
  group_by(pflanzjahr, quartier) %>%
  summarise(tree_count = n(), crown_sum = sum(kronendurchmesser), total_sum = cumsum(tree_count))

tree_year_quartier_sum <- tree_year_quartier %>%
  mutate(cum_trees = cumsum(tree_count)) %>%
  mutate(cum_crown = cumsum(crown_sum))


year_list <- do.call(seq, as.list(range(c(1983, 2023))))

tree_year_quartier$pflanzjahr

tree_year_quartier2 <- tree_year_quartier %>%
  ungroup(pflanzjahr) %>%
  dplyr::select(quartier) %>%
  expand(quartier, year_list) %>%
  rename(pflanzjahr = year_list) %>%
  full_join(tree_year_quartier, by = c('pflanzjahr', 'quartier')) %>%
  arrange(quartier, pflanzjahr) %>%
  filter(pflanzjahr > 1982) %>%
  mutate(tree_count = replace_na(tree_year_quartier2$tree_count, 0))

tree_year_quartier2$tree_sum <- 0



for (x in 2:length(tree_year_quartier2$quartier)) {
  if (tree_year_quartier2$quartier[x] == tree_year_quartier2$quartier[x-1]) {
    tree_year_quartier2$tree_sum[x] <- tree_year_quartier2$tree_count[x] + tree_year_quartier2$tree_sum[x-1]
  } else {
    tree_year_quartier2$tree_sum[x] <- tree_year_quartier2$tree_count[x]
  }
}

write_csv(tree_year_quartier2, 'tree_year_quartier_for_tableau.csv')














