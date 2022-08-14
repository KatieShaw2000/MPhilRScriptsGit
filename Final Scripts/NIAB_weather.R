#set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Phenotyping Campaign/2021 Weather Data")

#get packages needed ----

library(data.table)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(scales)

#get data ---- 

# file.list <- list.files(pattern='*.csv', recursive = TRUE)
# weather.list <- lapply(file.list, read.csv)
# data <- lapply(weather.list, tail, -6) #drop the first 6 rows of each csv file 
# weather <- rbindlist(data, fill = TRUE)
# 
# rm(file.list, weather.list, data)
# 
# #edit data frame to format I want ----
# 
# names(weather)[1] <- "Date"
# names(weather)[2] <- "Time"
# weather <- weather[,c(1,2,7,15)]
# names(weather)[3] <- "grass_temp"
# names(weather)[4] <- "humidity"
# 
# weather$grass_temp <- as.numeric(weather$grass_temp)
# weather$humidity <- as.numeric(weather$humidity)
# 
# weather$Date <- dmy(weather$Date)
# weather$Time <- substr(weather$Time, start = 1, stop = 2)
# weather$Time <- as.numeric(weather$Time)
# 
# weather <- weather[order(weather$Date),]
# 
# #get mean day temperatures and night temperatures etc for thesis methods ----
# #day and night times worked out based on sunrise and sunset times 
# 
# day <- subset(weather, Time > 5)
# day <- subset(day, Time < 23)
# 
# night1 <- subset(weather, Time < 6)
# night2 <- subset(weather, Time > 22)
# night <- rbind(night1, night2)
# night <- night[order(night$Date),]
# 
# rm(night1, night2)
# 
# range(day$grass_temp)
# range(day$humidity)
# range(night$grass_temp)
# range(night$humidity)
# 
# mean(day$grass_temp)
# mean(night$grass_temp)
# mean(night$humidity)
# mean(day$humidity)
# 
# sd(day$grass_temp)

#look at light data ----

light <- read.csv("Light.csv")

light <- light[19801:28260,] #get in the date range I want

light$Light <- as.numeric(light$Light)

light$Time <- dmy_hm(light$Time)

ggplot(light, aes(x=Time, y=Light)) + geom_line() +
  ylab("Light Intensity (units)") +
  xlab("Date") +
  scale_x_continuous(breaks = pretty(light$Time, n = 20)) #maybe too many points here? try with half the number of data points

light <- light %>% filter(row_number() %% 2 != 0) #delete every second row
light <- light %>% filter(row_number() %% 2 != 0) #and again -- gives a reading every 32 minutes

ggplot(light, aes(x=Time, y=Light)) + geom_line(col="blue") +
  ylab(~paste("PAR (", mu, "mol m"^-2,"s"^-1,')')) +
  xlab("Date") +
  scale_x_continuous(labels = c("26-Apr", "03-May", "10-May", "17-May", "24-May", "31-May", "07-Jun", "14-Jun"), breaks = pretty(light$Time, n = 15)) +
  theme(axis.text.x=element_text(angle=60, hjust=1, size=10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

#look at temp data ----

temp <- read.csv("Temp.csv")

temp <- temp[19801:28260,] #get in the date range I want

temp$Temp <- as.numeric(temp$Temp)

temp$Time_hours <- substr(temp$Time, start = 12, stop = 13)
temp$Time_hours <- as.numeric(temp$Time_hours)

day <- subset(temp, Time_hours > 5)
day <- subset(day, Time_hours < 23)

night1 <- subset(temp, Time_hours < 6)
night2 <- subset(temp, Time_hours > 22)
night <- rbind(night1, night2)
night <- night[order(night$Time),]
rm(night1, night2)
 
mean(day$Temp)
sd(day$Temp)

mean(night$Temp)
sd(night$Temp)
