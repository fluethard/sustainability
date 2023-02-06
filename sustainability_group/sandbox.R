### Script zum Ausprobieren ####################################################

library(tidyr)      # %>%
library(dplyr)      # select
library(ggplot2)    # ggplot
library(reshape2)   # melt


# load all meteo data within the folder into the same dataframe 
#------------------------------------------------------------------------------#
# source: https://chat.openai.com/chat :-)

# !!!!   set working directory to source file location    !!!!
filenames.meteo <- list.files(path = "../datasets/meteo/", pattern = "*.csv", full.names = TRUE)
data_list.meteo <- lapply(filenames.meteo, read.csv)
# Combine the data frames in the list into a single data frame
meteo <- do.call(rbind, data_list.meteo)

names(meteo)[1] <- "Datum"                                # Elisabeth needs this

meteo <- meteo[meteo$Standort=="Zch_Stampfenbachstrasse",]
meteo <- meteo %>% 
  select(c("Datum","Parameter","Wert")) %>%
  pivot_wider(id_cols = "Datum",names_from = "Parameter",values_from = "Wert") %>%
  select(c("Datum","T","RainDur","p"))

meteo$Datum <- format(as.Date(meteo$Datum), format = "%Y-%m-%d")
colnames(meteo) <-c("Datum","Temperatur", "RegenDauer", "Luftdruck")
plot(meteo$Temperatur, type = "l")



# load all air data within the folder into the same dataframe 
#------------------------------------------------------------------------------#

# !!!!   set working directory to source file location    !!!!

filenames.air <- list.files(path = "../datasets/air/", pattern = "*.csv", full.names = TRUE)
#filenames <- list.files(path = "path/to/folder", pattern = "*.csv", full.names = TRUE)
# Load each file into a separate data frame and store the data frames in a list
data_list.air <- lapply(filenames.air, read.csv)
# Combine the data frames in the list into a single data frame
air <- do.call(rbind, data_list.air)
names(air)[1] <- "Datum"                                # Elisabeth needs this
air$Datum <- as.Date(format(air$Datum), "%Y-%m-%d")

air <- air[air$Standort=="Zch_Stampfenbachstrasse",]

air <- air %>% 
  select(c("Datum", "Parameter", "Wert")) %>%
  pivot_wider(id_cols = "Datum",names_from = "Parameter",values_from = "Wert") 

#plot(air$CO, type = "l")

# plot air data 
#------------------------------------------------------------------------------#
# get rid of columns with lots of na
air.short <- air %>% 
  select(c(-"O3", -"O3_max_h1", -"O3_nb_h1>120", -"PM10", -"PN", -"PM2.5"))

#melt data frame into long format
air.plot <- melt(air.short,  id.vars = "Datum")
#create line plot for each column in data frame
ggplot(air.plot, aes(Datum, value), group = 5) +
  geom_line(aes(colour = variable))

# try with all variables - needs beautifying...
air.plot.long <- melt(air,  id.vars = "Datum")
#create line plot for each column in data frame
ggplot(air.plot.long, aes(Datum, value), group = 11) +
  geom_line(aes(colour = variable))
