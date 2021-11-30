#Set working directory ----

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/CF files")

#Get packages ----

library(readxl)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(ggforce)
library(tidyverse)

#Trying a loop ----

files <- list.files(pattern = "*.csv", recursive=TRUE)

plates <- c("Plot", "Repeat", "QY_max", "Fv/Fm_names", "Fv/Fm_values", "Fv/Fm_names", "Fv/Fm_values", "Filename")

for (file in files) {
  plate <- read.csv(file, skip=2) #read and remove weird first two lines
  plate <- t(plate) #transpose
  plate <- data.frame(plate) #make into dataframe
  rownames(plate) <- NULL #remove rownames
  colnames(plate) <- plate[1,] 
  plate <- plate[-1,]
  
  plate <- select(plate,"Plot", "Repeat", "QY_max", 
                  "Fv/Fm_L1","Fv/Fm_L2","Fv/Fm_L3","Fv/Fm_L4","Fv/Fm_L5","Fv/Fm_L6","Fv/Fm_L7","Fv/Fm_L8","Fv/Fm_L9","Fv/Fm_L10","Fv/Fm_L11",
                  "Fv/Fm_Lss", "Fv/Fm_D1","Fv/Fm_D2","Fv/Fm_D3","Fv/Fm_D4","Fv/Fm_D5","Fv/Fm_D6","Fv/Fm_D7","Fv/Fm_D8",
                  "NPQ_L1","NPQ_L2","NPQ_L3","NPQ_L4","NPQ_L5","NPQ_L6","NPQ_L7","NPQ_L8","NPQ_L9","NPQ_L10","NPQ_L11", 
                  "NPQ_Lss", "NPQ_D1","NPQ_D2","NPQ_D3","NPQ_D4","NPQ_D5","NPQ_D6","NPQ_D7","NPQ_D8")
  
  plate_a <- pivot_longer(plate, cols=4:23, names_to="Fv/Fm_names", values_to ="Fv/Fm_values")
  plate_a <- select(plate_a, "Plot", "Repeat", "QY_max", "Fv/Fm_names", "Fv/Fm_values")
  
  plate_b <- pivot_longer(plate, cols=24:43, names_to = "NPQ_names", values_to = "NPQ_values")
  
  plate_a$NPQ_names <- plate_b$NPQ_names
  plate_a$NPQ_values <- plate_b$NPQ_values
  
  plates <- rbind(plates, plate_a)
}

output <- plates[-1,]

output$QY_max <- as.numeric(output$QY_max)
output$`Fv/Fm_values` <- as.numeric(output$`Fv/Fm_values`)
output$NPQ_values <- as.numeric(output$NPQ_values)
output$Repeat <- as.factor(output$Repeat)

time_post_light_on <- c(20,40,60,120,180,240,300,360,420,480,540,600,NA,NA,NA,NA,NA,NA,NA,NA)
time_post_light_off <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,20,40,60,120,180,360,540,720)
cumulative_time <- c(20,40,60,120,180,240,300,360,420,480,540,600,620,640,660,720,780,960,1140,1320)

output$time_post_light_on <- rep(time_post_light_on, times=1548)
output$time_post_light_off <- rep(time_post_light_off, times=1548)
output$cumulative_time <- rep(cumulative_time, times=1548)

#visualise ----

ggplot(data=output, aes(x=cumulative_time, y=NPQ_values, colour=Repeat)) +
  geom_point()+
  facet_wrap_paginate( ~ Plot, ncol= 5, nrow = 5, page = 1)

ggplot(data=output, aes(x=cumulative_time, y=`Fv/Fm_values`, colour=Repeat)) +
  geom_point()+
  facet_wrap_paginate( ~ Plot, ncol= 8, nrow = 5, page = 1)