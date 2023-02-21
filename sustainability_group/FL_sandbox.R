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


#summarise CO2 by year
yearly_air <- air %>%
  add_column(year = lubridate::year(air$Datum))

yearly_aq <- yearly_air %>%
  #dplyr::select(year, CO) %>%
  group_by(year) %>%
  summarise(mean_co = mean(CO, na.rm = T), mean_no2 = mean(NO2, na.rm = T), mean_no = mean(NO, na.rm = T), mean_nox = mean(NOx, na.rm = T), mean_so2 = mean(SO2, na.rm = T))





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




temp_by_station_2022 <- merged_temp %>%
  add_column(year = lubridate::year(merged_temp$date)) %>%
  add_column(quartier = case_when(merged_temp$station == 'Zch_Stampfenbachstrasse' ~ 'Rathaus', merged_temp$station == 'Zch_Rosengartenstrasse' ~ 'Wipkingen', merged_temp$station == 'Zch_Schimmelstrasse' ~ 'Sihlfeld')) %>%
  filter(measurement == "T_max_h1") %>%
  filter(year == '2022') %>%
  dplyr::select(date, station, quartier, temperature)

  write_csv(temp_by_station_2022, 'temp_station_for_tableau_2022.csv')


  
  
  


  # New station data from Salomon Idaweb
  
temp_affoltern <- read.csv('/Users/fluethard/hslu/sustainability/git/sustainability/datasets/temp_affoltern.csv', sep = ';', fill = T)
  
temp_fluntern <- read.csv('/Users/fluethard/hslu/sustainability/git/sustainability/datasets/temp_fluntern.csv', sep = ';')

read.csv

ref_temp_fluntern <- temp_fluntern %>%
  dplyr::select(stn, time, ths200dx, ths200d0)

ref_temp_fluntern <- ref_temp_fluntern %>%
  rename(c(temp_mean = ths200d0, temp_max =ths200dx)) %>%
  mutate(time = lubridate::ymd(time))
  

ref_temp_fluntern_year <- ref_temp_fluntern %>%
  add_column(year = lubridate::year(ref_temp_fluntern$time)) %>%
  dplyr::select(year, stn, temp_mean) %>%
  group_by(year, stn) %>%
  summarise(mean_temp = mean(temp_mean, na.rm = T)) %>%
  filter(year > 1991) %>%
  filter(year < 2023) %>%
  rename(station = stn)

year_temp_by_station_all <- rbind(year_temp_by_station, ref_temp_fluntern_year)

year_temp_by_station_all <- year_temp_by_station_all %>%
  add_column(quartier = case_when(year_temp_by_station_all$station == 'Zch_Stampfenbachstrasse' ~ 'Rathaus', year_temp_by_station_all$station == 'Zch_Rosengartenstrasse' ~ 'Wipkingen', year_temp_by_station_all$station == 'Zch_Schimmelstrasse' ~ 'Sihlfeld', year_temp_by_station_all$station == 'SMA' ~ 'Fluntern'))

write_csv(year_temp_by_station_all, 'year_temp_by_station_all_for_tableau.csv')




yearly_nox <- air %>%
  add_column(quartier = case_when(air$Standort == 'Zch_Stampfenbachstrasse' ~ 'Rathaus', air$Standort == 'Zch_Rosengartenstrasse' ~ 'Wipkingen', air$Standort == 'Zch_Schimmelstrasse' ~ 'Sihlfeld', air$Standort == 'Zch_Heubeeribüel' ~ 'Fluntern')) %>%
  filter(Parameter == 'NOx') %>%
  dplyr::select(Jahr, Standort, quartier, Wert) %>%
  group_by(Jahr, Standort, quartier) %>%
  summarise(mean_nox = mean(Wert, na.rm = T)) %>%
  rename(year = Jahr, station = Standort) %>%
  filter(year < 2023)



write_csv(yearly_nox, 'year_nox_tableau.csv')







  
  


