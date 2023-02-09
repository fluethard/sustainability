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


#summarise CO2 by year
yearly_air <- air %>%
  add_column(year = lubridate::year(air$Datum))

yearly_aq <- yearly_air %>%
  #dplyr::select(year, CO) %>%
  group_by(year) %>%
  summarise(mean_co = mean(CO, na.rm = T), mean_no2 = mean(NO2, na.rm = T), mean_no = mean(NO, na.rm = T), mean_nox = mean(NOx, na.rm = T), mean_so2 = mean(SO2, na.rm = T))

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



merged_temp <- read_csv("/Users/fluethard/hslu/sustainability/git/sustainability/datasets/merged_temp.csv")

names(merged_temp)[1] <- "date"
names(merged_temp)[2] <- "station"
names(merged_temp)[3] <- "measurement"
names(merged_temp)[4] <- "date_unit"
names(merged_temp)[5] <- "unit"
names(merged_temp)[6] <- "temperature"


year_temp_stampfenbach <- merged_temp %>%
  add_column(year = lubridate::year(merged_temp$date)) %>%
  filter(station == "Zch_Stampfenbachstrasse", measurement == "T") %>%
  dplyr::select(year, temperature) %>%
  group_by(year) %>%
  summarise(mean_temp = mean(temperature, na.rm = T))


year_temp_by_station <- merged_temp %>%
  add_column(year = lubridate::year(merged_temp$date)) %>%
  filter(measurement == "T") %>%
  dplyr::select(year, station, temperature) %>%
  group_by(year, station) %>%
  summarise(mean_temp = mean(temperature, na.rm = T))


temp_station <- merged_temp %>%
  add_column(year = lubridate::year(merged_temp$date)) %>%
  filter(measurement == "T_max_h1") %>%
  filter(year == '2022') %>%
  dplyr::select(year, station, temperature)

write_csv(temp_station, 'temp_station_for_tableau.csv')

write_csv(tree_year_quartier, 'tree_year_quartier_for_tableau.csv')


temp_by_station_2022 <- merged_temp %>%
  add_column(year = lubridate::year(merged_temp$date)) %>%
  add_column(quartier = case_when(merged_temp$station == 'Zch_Stampfenbachstrasse' ~ 'Rathaus', merged_temp$station == 'Zch_Rosengartenstrasse' ~ 'Wipkingen', merged_temp$station == 'Zch_Schimmelstrasse' ~ 'Sihlfeld')) %>%
  filter(measurement == "T_max_h1") %>%
  filter(year == '2022') %>%
  dplyr::select(date, station, quartier, temperature)

  write_csv(temp_by_station_2022, 'temp_station_for_tableau_2022.csv')


  
  
  


  
  



