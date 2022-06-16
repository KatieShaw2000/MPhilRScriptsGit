#set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Phenotyping Campaign/2021 Weather Data")

#get packages needed ----

library(data.table)
library(lubridate)

#get data ---- 

file.list <- list.files(pattern='*.csv', recursive = TRUE)
weather.list <- lapply(file.list, read.csv)
data <- lapply(weather.list, tail, -6) #drop the first 6 rows of each csv file 
weather <- rbindlist(data, fill = TRUE)

rm(file.list, weather.list, data)

#edit data frame to format I want ----

names(weather)[1] <- "Date"
names(weather)[2] <- "Time"
weather <- weather[,c(1,2,7,15)]
names(weather)[3] <- "grass_temp"
names(weather)[4] <- "humidity"

weather$grass_temp <- as.numeric(weather$grass_temp)
weather$humidity <- as.numeric(weather$humidity)

weather$Date <- dmy(weather$Date)
weather$Time <- substr(weather$Time, start = 1, stop = 2)
weather$Time <- as.numeric(weather$Time)

weather <- weather[order(weather$Date),]

#get mean day temperatures and night temperatures etc for thesis methods ----
#day and night times worked out based on sunrise and sunset times 

day <- subset(weather, Time > 5)
day <- subset(day, Time < 23)

night1 <- subset(weather, Time < 6)
night2 <- subset(weather, Time > 22)
night <- rbind(night1, night2)
night <- night[order(night$Date),]

rm(night1, night2)

range(day$grass_temp)
range(day$humidity)
range(night$grass_temp)
range(night$humidity)

mean(day$grass_temp)
mean(night$grass_temp)
mean(night$humidity)
mean(day$humidity)
