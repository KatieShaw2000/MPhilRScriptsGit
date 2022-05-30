##Looking at the tiny tag and light intensity data!!##

#Set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Experimental Work/Drought Experiment")

#Packages needed ----
library(readxl)
library(ggplot2)
library(hms)
library(ggpubr)

#Get tiny tag data ----

tiny_tag <- read_excel("TinyTag_Glasshouse.xlsx")
tiny_tag <- tiny_tag[,c(5,6,2,3)] #get data frame in order I want

names(tiny_tag)[1] <- "Date" #get right column names
names(tiny_tag)[2] <- "Time"
names(tiny_tag)[3] <- "Temperature"
names(tiny_tag)[4] <- "Humidity"

tiny_tag <- tiny_tag[-c(1:4),]

tiny_tag$Temperature <- as.numeric(tiny_tag$Temperature) #make temperature numeric
tiny_tag$Humidity <- as.numeric(tiny_tag$Humidity) #make humidity numeric
tiny_tag$Date <- as.Date(tiny_tag$Date)

tiny_tag$Time <- substr(tiny_tag$Time, 12, 16) #get time in appropriate layout

#split data into day and night - make hour column and then use this for inequalities!

tiny_tag$Hour <- substr(tiny_tag$Time, 1,2) #getting hour column
tiny_tag$Hour <- as.numeric(tiny_tag$Hour)

tiny_tag_day <- subset(tiny_tag, Hour >= 4 & Hour <= 19) #get times between 4am and before 9pm
tiny_tag_night <- subset(tiny_tag, !(Hour >= 4 & Hour <= 19)) #get all other times

#Get light intensity data ----

light_data <- read.csv("Light_Intensity_Glasshouse.csv")

light_data$Date <- substr(light_data$Label, 1, 10) #get date and time from column
light_data$Time <- substr(light_data$Label, 12, 16)

light_data <- light_data[-c(1:3410),c(6,7,2,3,4,5)] #get data from my dates in the glasshouse and data frame in the order I want

light_data$Date <- as.Date(light_data$Date)
light_data$Total <- as.numeric(light_data$Total)

light_data$Hour <- substr(light_data$Time, 1,2) #getting hour column
light_data$Hour <- as.numeric(light_data$Hour)

light_data_day <- subset(light_data, Hour >= 4 & Hour <= 19) #get times between 4am and before 9pm
light_data_night <- subset(light_data, !(Hour >= 4 & Hour <= 19)) #get all other times

#Temperature data ----

day_temp <- data.frame(Time = "Day", value = tiny_tag_day$Temperature) #need to do this so I can plot night/day on same boxplot
night_temp <- data.frame(Time = "Night", value = tiny_tag_night$Temperature)
temp <- rbind(day_temp, night_temp)

temp_boxplot <- ggplot(temp, aes(x=Time, y=value, fill=Time)) + geom_boxplot() + theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(size=0.1, color="black"), 
        panel.grid.minor.y = element_line(size=0.1, color="black")) + 
  xlab("") + ylab(expression("Temperature " ( degree*C))) + theme(legend.position = "none")

ggplot(tiny_tag, aes(x=Time, y=Temperature)) + geom_point() +
  theme(axis.text.x = element_text(angle=90)) 

#RH data ----

day_RH <- data.frame(Time = "Day", value = tiny_tag_day$Humidity)
night_RH <- data.frame(Time = "Night", value = tiny_tag_night$Humidity)
RH <- rbind(day_RH, night_RH)
RH <- RH[RH$value >= 10,]
RH <- RH[RH$value <= 90,]

RH_boxplot <- ggplot(RH, aes(x=Time, y=value, fill=Time)) + geom_boxplot() + theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(size=0.1, color="black"), 
        panel.grid.minor.y = element_line(size=0.1, color="black")) + 
  xlab("") + ylab("Relative Humidity (%)") + theme(legend.position = "none")

ggplot(tiny_tag, aes(x=Time, y=Humidity)) + geom_point() +
  theme(axis.text.x = element_text(angle=90)) 

#Light intensity data ----

day_LI <- data.frame(Time = "Day", value = light_data_day$Total)
night_LI <- data.frame(Time = "Night", value = light_data_night$Total)
LI <- rbind(day_LI, night_LI)

LI_boxplot <- ggplot(LI, aes(x=Time, y=value, fill=Time)) + geom_boxplot() + theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(size=0.1, color="black"), 
        panel.grid.minor.y = element_line(size=0.1, color="black")) + 
  xlab("") + ylab(bquote('Total Solar Radiation ('*mu~'mol'~m^-2~s^-1*')'))

ggplot(light_data, aes(x=Time, y=Total)) + geom_point() +
  theme(axis.text.x = element_text(angle=90)) 

#Combining all data for a plot ----

ggarrange(temp_boxplot, RH_boxplot, LI_boxplot, labels = c("A", "B", "C"), ncol = 3, nrow = 1)

#getting some ranges/mean values for my writing ----

mean(tiny_tag_day$Temperature)
mean(tiny_tag_night$Temperature)

quantile(tiny_tag_day$Temperature, prob=c(0.25, 0.5, 0.75))
quantile(tiny_tag_night$Temperature, prob=c(0.25, 0.5, 0.75))

tiny_tag_day <- tiny_tag_day[tiny_tag_day$Humidity >=10,]
tiny_tag_day <- tiny_tag_day[tiny_tag_day$Humidity <=90,]
tiny_tag_night <- tiny_tag_night[tiny_tag_night$Humidity >=10,]
tiny_tag_night <- tiny_tag_night[tiny_tag_night$Humidity <=90,]

mean(tiny_tag_day$Humidity)
mean(tiny_tag_night$Humidity)

quantile(tiny_tag_day$Humidity, prob=c(0.25, 0.5, 0.75))
quantile(tiny_tag_night$Humidity, prob=c(0.25, 0.5, 0.75))

light_data_day <- na.omit(light_data_day)
mean(light_data_day$Total)
mean(light_data_night$Total)