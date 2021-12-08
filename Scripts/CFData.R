#Set working directory ----

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/CF files")

#Get packages ----

library(readxl)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(ggforce)
library(tidyverse)
library(dplyr)
library(writexl)

#Trying a loop ----

files <- list.files(pattern = "*.csv", recursive=TRUE)

plates <- c("Plot", "Repeat", "QY_max", "Fv/Fm_names", "Fv/Fm_values", "Fv/Fm_names", "Fv/Fm_values")

for (file in files) {
  plate <- read.csv(file, skip=2) #read and remove weird first two lines
  plate <- t(plate) #transpose
  plate <- data.frame(plate) #make into dataframe
  rownames(plate) <- NULL #remove rownames
  colnames(plate) <- plate[1,] 
  plate <- plate[-1,]
  
  plate$NPQ_L0 <- 0 #adding in a t=0 timepoint
  plate$NPQ_L0 <- as.character(plate$NPQ_L0) #allows it to work 
  plate$`Fv/Fm_L0` <- NA
  
  plate <- select(plate,"Plot", "Repeat", "QY_max", "Fv/Fm_L0",
                  "Fv/Fm_L1","Fv/Fm_L2","Fv/Fm_L3","Fv/Fm_L4","Fv/Fm_L5","Fv/Fm_L6","Fv/Fm_L7","Fv/Fm_L8","Fv/Fm_L9","Fv/Fm_L10","Fv/Fm_L11",
                  "Fv/Fm_Lss", "Fv/Fm_D1","Fv/Fm_D2","Fv/Fm_D3","Fv/Fm_D4","Fv/Fm_D5","Fv/Fm_D6","Fv/Fm_D7","Fv/Fm_D8",
                  "NPQ_L0", "NPQ_L1","NPQ_L2","NPQ_L3","NPQ_L4","NPQ_L5","NPQ_L6","NPQ_L7","NPQ_L8","NPQ_L9","NPQ_L10","NPQ_L11", 
                  "NPQ_Lss", "NPQ_D1","NPQ_D2","NPQ_D3","NPQ_D4","NPQ_D5","NPQ_D6","NPQ_D7","NPQ_D8")
  
  plate_a <- pivot_longer(plate, cols=4:24, names_to="Fv/Fm_names", values_to ="Fv/Fm_values")
  plate_a <- select(plate_a, "Plot", "Repeat", "QY_max", "Fv/Fm_names", "Fv/Fm_values")
  
  plate_b <- pivot_longer(plate, cols=25:45, names_to = "NPQ_names", values_to = "NPQ_values")
  
  plate_a$NPQ_names <- plate_b$NPQ_names
  plate_a$NPQ_values <- plate_b$NPQ_values
  
  plates <- rbind(plates, plate_a)
}

output <- plates[-1,]

output$QY_max <- as.numeric(output$QY_max)
output$`Fv/Fm_values` <- as.numeric(output$`Fv/Fm_values`)
output$NPQ_values <- as.numeric(output$NPQ_values)
output$Repeat <- as.factor(output$Repeat)
names(output)[4] <- paste("Fv/Fm")
output$Plot <- as.numeric(output$Plot)

time_post_light_on <- c(0,20,40,60,120,180,240,300,360,420,480,540,600,NA,NA,NA,NA,NA,NA,NA,NA)
time_post_light_off <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,0,20,40,60,120,180,360,540,720)
cumulative_time <- c(0,20,40,60,120,180,240,300,360,420,480,540,600,620,640,660,720,780,960,1140,1320)

output$time_post_light_on <- rep(time_post_light_on, times=1548)
output$time_post_light_off <- rep(time_post_light_off, times=1548)
output$cumulative_time <- rep(cumulative_time, times=1548)

#Cleaning data ----

#Looking at QY_max spread to filter all data 

QY_max <- output[seq(1,nrow(output), 21),] #remove all the repeats for QY_max
QY_max <- select(QY_max, "Plot", "Repeat", "QY_max")

ggplot(data=QY_max, aes(x=QY_max)) + geom_density() + 
  geom_vline(xintercept=0.78, colour = "red") #density plot

ggplot(data=QY_max, aes(x=factor(0), y=QY_max)) +
  geom_boxplot() +
  geom_jitter(position=position_jitter(0.2)) #boxplot

#filter based on QY_max -- based on plots, suggest threshold of 0.78?

filtered_output <- filter(output, QY_max >= 0.78)
filtered_output <- filter(filtered_output, Plot != "unknown") #remove unknown/unlabelled plots

#visualise ----

ggplot(data=filtered_output, aes(x=cumulative_time, y=NPQ_values, colour=Repeat)) +
  geom_point()+
  facet_wrap_paginate( ~ Plot, ncol= 10, nrow = 8, page = 2)

#export the filtered output to load into other script to do some fitting 

write_xlsx(filtered_output, path= "~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/NPQdata.xlsx")
