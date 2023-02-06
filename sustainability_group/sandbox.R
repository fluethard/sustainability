### Script zum Ausprobieren ####################################################

library(tidyr)  # %>%
library(dplyr)  # select

# load all meteo data within the folder into the same dataframe 
#------------------------------------------------------------------------------#
# source: https://chat.openai.com/chat :-)

# !!!!   set working directory to source file location    !!!!
filenames <- list.files(path = "../datasets/meteo/", pattern = "*.csv", full.names = TRUE)
data_list <- lapply(filenames, read.csv)
# Combine the data frames in the list into a single data frame
meteo <- do.call(rbind, data_list)

names(meteo)[1] <- "Datum"                                # Elisabeth needs this

meteo <- meteo[meteo$Standort=="Zch_Stampfenbachstrasse",]
meteo <- meteo %>% 
  select(c("Datum","Parameter","Wert")) %>%
  pivot_wider(id_cols = "Datum",names_from = "Parameter",values_from = "Wert") %>%
  select(c("Datum","T","RainDur","p"))

meteo$Datum <- format(as.Date(meteo$Datum), format = "%Y-%m-%d")
colnames(meteo) <-c("Datum","Temperatur", "RegenDauer", "Luftdruck")
plot(meteo$Temperatur, type = "l")
