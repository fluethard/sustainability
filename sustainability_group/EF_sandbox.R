### Script zum Ausprobieren ####################################################

library(tidyr)      # %>%
library(dplyr)      # select, filter
library(ggplot2)    # ggplot
library(reshape2)   # melt
library(lubridate)  # year


# load meteo data within the folder into the same dataframe 
#------------------------------------------------------------------------------#
# source: https://chat.openai.com/chat :-)

# # !!!!   set working directory to source file location    !!!!
# filenames.meteo <- list.files(path = "../datasets/meteo/", pattern = "*.csv", full.names = TRUE)
# data_list.meteo <- lapply(filenames.meteo, read.csv)
# # Combine the data frames in the list into a single data frame
# meteo <- do.call(rbind, data_list.meteo)
# 
# names(meteo)[1] <- "Datum"                                # Elisabeth needs this
# 
# meteo <- meteo[meteo$Standort=="Zch_Stampfenbachstrasse",]
# meteo <- meteo %>% 
#   select(c("Datum","Parameter","Wert")) %>%
#   pivot_wider(id_cols = "Datum",names_from = "Parameter",values_from = "Wert") %>%
#   select(c("Datum","T","RainDur","p"))
# 
# meteo$Datum <- format(as.Date(meteo$Datum), format = "%Y-%m-%d")
# colnames(meteo) <-c("Datum","Temperatur", "RegenDauer", "Luftdruck")
# plot(meteo$Temperatur, type = "l")


################################################################################
# from .Rmd  load meteo  
filenames.meteo <- list.files(path = "../datasets/meteo/", pattern = "*.csv", full.names = TRUE)
data_list.meteo <- lapply(filenames.meteo, read.csv)
# Combine the data frames in the list into a single data frame
meteo <- do.call(rbind, data_list.meteo)

names(meteo)[1] <- "Date"
meteo$Date <- format(as.Date(meteo$Date), format = "%Y-%m-%d")


# Datensatz mit Temperatur und Globalstrahlung pro Messstation
meteo.extr <- meteo %>%
  filter(Parameter == "T" | Parameter == "StrGlo" | Parameter =="T_max_h1")
meteo.extr$Jahr <- year(meteo.extr$Date)


# weiter mit dem normalen meteo-Datensatz
meteo <- meteo[meteo$Standort=="Zch_Stampfenbachstrasse",]
meteo <- meteo %>% 
  select(c("Date","Parameter","Wert")) %>%
  pivot_wider(id_cols = "Date",names_from = "Parameter",values_from = "Wert") %>%
  select(c("Date","T","RainDur","p"))

meteo$Date <- format(as.Date(meteo$Date), format = "%Y-%m-%d")
meteo$Date <- ymd(meteo$Date)


head(meteo)




# load air data within the folder into the same dataframe 
#------------------------------------------------------------------------------#

# !!!!   set working directory to source file location    !!!!

filenames.air <- list.files(path = "../datasets/air/", pattern = "*.csv", full.names = TRUE)
#filenames <- list.files(path = "path/to/folder", pattern = "*.csv", full.names = TRUE)
# Load each file into a separate data frame and store the data frames in a list
data_list.air <- lapply(filenames.air, read.csv, encoding = "utf-8")
# Combine the data frames in the list into a single data frame
air <- do.call(rbind, data_list.air)
names(air)[1] <- "Datum"                                # Elisabeth needs this
air$Datum <- as.Date(format(air$Datum), "%Y-%m-%d")

table(air$Standort)             # Zch_Stampfenbachstrasse ist am vollständigsten

air.wide <- air %>% 
  filter(Standort == "Zch_Stampfenbachstrasse") %>%
  select(c("Datum", "Parameter", "Wert")) %>%
  pivot_wider(id_cols = "Datum", names_from = "Parameter", values_from = "Wert") 

meteo <- meteo %>% 
  pivot_wider(id_cols = "Datum",names_from = "Parameter",values_from = "Wert")

#plot(air$CO, type = "l")


# plot air data 
#------------------------------------------------------------------------------#
# get rid of columns with lots of NA
air.short <- air.wide %>% 
  select(c(-"O3", -"O3_max_h1", -"O3_nb_h1>120", -"PM10", -"PN", -"PM2.5"))

#melt data frame into long format
air.plot <- melt(air.short,  id.vars = "Datum")
#create line plot for each column in data frame
ggplot(air.plot, aes(Datum, value), group = 5) +
  geom_line(aes(colour = variable))

# try with all variables - needs beautifying...
air.plot.long <- melt(air,  id.vars = "Datum")
# create line plot for each column in data frame
# ggplot(air.plot.long, aes(Datum, value), group = 11) +
#   geom_line(aes(colour = variable))

plot(air$CO, type = "l")
plot(air$NOx, type = "l")



################################################################################
# EDA - Exploratory Data analysis


# Mean der Luftschadstoffe pro Jahr
#------------------------------------------------------------------------------#
air$Jahr <- as.numeric(format(air$Datum,'%Y'))

air.means = air %>% 
  filter(!is.na(Wert)) %>% 
  group_by(Parameter, Jahr) %>%
  summarize(Mean = round(mean(Wert),2),
            .groups = "drop")

table(air.means$Parameter)


# line graph per mean 
#------------------------------------------------------------------------------#
plot(subset(air.means, Parameter == "CO", 
            c(Jahr, Mean)), type = "l",
     main = "Mean of CO per Year [mg/m3]")  

plot(subset(air.means, Parameter == "NO", 
            c(Jahr, Mean)), type = "l",
     main = "Mean of NO per Year [µg/m3]")

plot(subset(air.means, Parameter == "NO2", 
            c(Jahr, Mean)), type = "l",
     main = "Mean of NO2 per Year [µg/m3]")

plot(subset(air.means, Parameter == "NOx", 
            c(Jahr, Mean)), type = "l",
     main = "Mean of NOx per Year [ppb]")

plot(subset(air.means, Parameter == "O3", 
            c(Jahr, Mean)), type = "l",
     main = "Mean of O3 per Year [µg/m3]") 

plot(subset(air.means, Parameter == "O3_max_h1",
            c(Jahr, Mean)), type = "l",
     main = "Mean of O3_max_h1 per Year [µg/m3]") 

plot(subset(air.means, Parameter == "O3_nb_h1>120",
            c(Jahr, Mean)), type = "l",
     main = "Mean of O3_nb_h1>120 per Year [1]") 

plot(subset(air.means, Parameter == "PM10",
            c(Jahr, Mean)), type = "l",
     main = "Mean of PM10 per Year [mg/m3]") 

plot(subset(air.means, Parameter == "PM2.5",
            c(Jahr, Mean)), type = "l",
     main = "Mean of PM2.5 per Year [mg/m3]") 

plot(subset(air.means, Parameter == "PN",
            c(Jahr, Mean)), type = "l",
     main = "Mean of PN per Year [1/cm3]") 

plot(subset(air.means, Parameter == "SO2",
            c(Jahr, Mean)), type = "l",
     main = "Mean of SO2 per Year [µg/m3]")


# Boxplots der Standorte
#------------------------------------------------------------------------------#
# Nützliche Übersichten
table(air$Standort)
table(air.he$Parameter)
table(air.ro$Parameter)
table(air.sc$Parameter)
table(air.st$Parameter)

# Boxplots for CO all over Zürich
boxplot(Wert ~ Jahr, data = air, subset = Parameter %in% names(table("CO")),
        main= "CO in Zürich (Stampfenbachstrasse) per Year [mg/m3]",
        xlab = "Year", ylab = "mg/m3")
boxplot(Wert ~ Jahr, data = air, subset = Parameter %in% names(table("NOx")),
        main= "NOx in Zürich per Year [ppb]",
        xlab = "Year", ylab = "ppb")

# Boxplots for NOx per monitoring station
air.he <- air %>% filter(Standort == "Zch_HeubeeribÃ¼el")
air.ro <- air %>% filter(Standort == "Zch_Rosengartenstrasse")
air.sc <- air %>% filter(Standort == "Zch_Schimmelstrasse")
air.st <- air %>% filter(Standort == "Zch_Stampfenbachstrasse")

boxplot(Wert ~ Jahr, data = air.he, subset = Parameter %in% names(table("NOx")),
        main= "NOx at Heubeeribuel per Year [ppb]",
        xlab = "Year", ylab = "ppb")
boxplot(Wert ~ Jahr, data = air.ro, subset = Parameter %in% names(table("NOx")),
        main= "NOx at Rosengartenstrasse per Year [ppb]",
        xlab = "Year", ylab = "ppb")
boxplot(Wert ~ Jahr, data = air.sc, subset = Parameter %in% names(table("NOx")),
        main= "NOx at Schimmelstrasse per Year [ppb]",
        xlab = "Year", ylab = "ppb")
boxplot(Wert ~ Jahr, data = air.st, subset = Parameter %in% names(table("NOx")),
        main= "NOx at Stampfenbachstrasse per Year [ppb]",
        xlab = "Year", ylab = "ppb")


# Trees, trees, trees
#------------------------------------------------------------------------------#
#load data
baum <- read.csv("../datasets/gsz.baumkataster_baumstandorte.csv", encoding = "utf-8")
names(baum)[1] <- "objid" 
nrow(baum)
str(baum)

# bar chart pflanzjahr alle
ggplot(baum, aes(x = pflanzjahr)) +
  geom_bar()
# bar chart nur neuere pflanzjahre
ggplot(baum[baum$pflanzjahr > 1949, ], aes(x = pflanzjahr)) +
  geom_bar()

# bar chart kronendurchmesser
ggplot(baum, aes(x = kronendurchmesser)) +
  geom_bar()


# Temperature: max/min over years
#------------------------------------------------------------------------------#
# meteo.extr %>%
#   max(Parameter == "T")

# example
meteo.minmax <- meteo.extr %>%
  group_by(Jahr, Parameter, Standort) %>%
  summarize(Max = max(Wert, na.rm = TRUE), .groups = "drop")

# subset(mydata, age >= 20 | age < 10, select=c(ID, Weight)) 

# meteo.minmax$Parameter <- as.factor(meteo.minmax$Parameter)
# meteo.minmax$Standort <- as.factor(meteo.minmax$Standort)

meteo.minmax.str <- meteo.minmax %>% filter(Parameter == "StrGlo")
meteo.minmax.tem <- meteo.minmax %>% filter(Parameter == "T")

#plot(meteo.minmax.str$Jahr, meteo.minmax.str$Max,  type = "l")
#plot("Jahr", "Max", data = meteo.minmax.str, type = "l")

# max Global Strahlung
ggplot(meteo.minmax.str, aes(x = Jahr, y = Max)) +
  geom_line() +
  ggtitle("Development of Global-Strahlung")

# max Temperatur je Messstation
ggplot(meteo.minmax.tem, aes(x = Jahr, y = Max)) +
  geom_line(aes(color = Standort, linetype = Standort)) +
  ggtitle("Development of temperature")


###
# Stampfenbach max of mean temperature with trendline
meteo.sta <- meteo.extr %>%
  filter(Standort == "Zch_Stampfenbachstrasse") %>%
  filter(Parameter == "T") %>%
  group_by(Jahr, Parameter) %>%
  summarize(Max = max(Wert, na.rm = TRUE), .groups = "drop")

ggplot(meteo.sta, aes(x = Jahr, y = Max)) +
  geom_line() +
  ggtitle("Development of maximum (of mean) temperature at Stampfenbachstrasse") +
  geom_smooth(method = "lm", se = FALSE)
summary(lm(meteo.sta$Max ~ meteo.sta$Jahr))

###
# Stampfenbach max of mean temperature with trendline
meteo.sta.min <- meteo.extr %>%
  filter(Standort == "Zch_Stampfenbachstrasse") %>%
  filter(Parameter == "T") %>%
  group_by(Jahr, Parameter) %>%
  summarize(Min = min(Wert, na.rm = TRUE), .groups = "drop")

ggplot(meteo.sta.min, aes(x = Jahr, y = Min)) +
  geom_line() +
  ggtitle("Development of minimum of mean temperature at Stampfenbachstrasse") +
  geom_smooth(method = "lm", se = FALSE)
summary(lm(meteo.sta.min$Min ~ meteo.sta.min$Jahr))

###
# Stampfenbach min of mean temperature with trendline
meteo.sta.max <- meteo.extr %>%
  filter(Standort == "Zch_Stampfenbachstrasse") %>%
  filter(Parameter == "T_max_h1") %>%
  group_by(Jahr, Parameter) %>%
  summarize(Max = max(Wert, na.rm = TRUE), .groups = "drop")

ggplot(meteo.sta.max, aes(x = Jahr, y = Max)) +
  geom_line() +
  ggtitle("Development of maximum daily temperature at Stampfenbachstrasse") +
  geom_smooth(method = "lm", se = FALSE)
summary(lm(meteo.sta.max$Max ~ meteo.sta.max$Jahr))

###
# Stampfenbach mean of Globalstrahlung with trendline
meteo.sta.glo <- meteo.extr %>%
filter(Standort == "Zch_Stampfenbachstrasse") %>%
  filter(Parameter == "StrGlo") %>%
  group_by(Jahr, Parameter) %>%
  summarize(Max = max(Wert, na.rm = TRUE), .groups = "drop")

ggplot(meteo.sta.glo, aes(x = Jahr, y = Max)) +
  geom_line() +
  ggtitle("Development of mean of global radiation temperature at Stampfenbachstrasse") +
  geom_smooth(method = "lm", se = FALSE)
summary(lm(meteo.sta.glo$Max ~ meteo.sta.glo$Jahr))


# Globalstrahlung
