#Set working directory

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/CF files")

#Get packages

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
  facet_wrap_paginate( ~ Plot, ncol= 8, nrow = 5, page = 1)

ggplot(data=output, aes(x=cumulative_time, y=`Fv/Fm_values`, colour=Repeat)) +
  geom_point()+
  facet_wrap_paginate( ~ Plot, ncol= 8, nrow = 5, page = 1)

#jun 1 ----

###

jun1_plate2 <- read.csv("June 1/plate2_june1.csv", skip =2, fill = TRUE)
jun1_plate2 <- t(jun1_plate2) #transpose 
jun1_plate2 <- data.frame(jun1_plate2) #make dataframe
rownames(jun1_plate2) <- NULL
colnames(jun1_plate2) <- jun1_plate2[1,]
jun1_plate2 <- jun1_plate2[-1,]

test <- pivot_longer(jun1_plate2, cols=4:23, names_to="Fv/Fm_names", values_to ="Fv/Fm_values")
test<- select(test, "Plot", "Repeat", "QY_max", "Fv/Fm_names", "Fv/Fm_values")

test2 <- pivot_longer(jun1_plate2, cols=24:43, names_to = "NPQ_names", values_to = "NPQ_values")
test2<- select(test2, "Plot", "Repeat", "QY_max", "NPQ_names", "NPQ_values")

test$NPQ_names <- test2$NPQ_names
test$NPQ_values <- test2$NPQ_values


###

jun1_plate3 <- read.delim("June 1/Plate 3.TXT", skip =2, fill = TRUE)
jun1_plate3 <- t(jun1_plate3) #transpose 
jun1_plate3 <- data.frame(jun1_plate3) #make dataframe
rownames(jun1_plate3) <- NULL
colnames(jun1_plate3) <- jun1_plate3[1,]
jun1_plate3 <- jun1_plate3[-1,]

jun1_plate3$Plot <- c(1194,1066,2067,1098,2214,2183,1281,2166,1320,1026,2214,2065,1202,2242,1192,1108,
                      1107,1025,2065,2091,2065,2067,1133,2183,1033,1227,2091,1163,2227,2067)
jun1_plate3$Repeat <- c(1,1,2,1,1,1,2,1,1,3,3,2,3,1,3,1,3,1,3,2,1,1,3,3,3,3,1,3,2,3)

jun1_plate3 <- select(jun1_plate3, "Plot", "Repeat", "QY_max", 
                      "Fv/Fm_L1","Fv/Fm_L2","Fv/Fm_L3","Fv/Fm_L4","Fv/Fm_L5","Fv/Fm_L6","Fv/Fm_L7","Fv/Fm_L8","Fv/Fm_L9","Fv/Fm_L10","Fv/Fm_L11",
                      "Fv/Fm_Lss", "Fv/Fm_D1","Fv/Fm_D2","Fv/Fm_D3","Fv/Fm_D4","Fv/Fm_D5","Fv/Fm_D6","Fv/Fm_D7","Fv/Fm_D8",
                      "NPQ_L1","NPQ_L2","NPQ_L3","NPQ_L4","NPQ_L5","NPQ_L6","NPQ_L7","NPQ_L8","NPQ_L9","NPQ_L10","NPQ_L11", 
                      "NPQ_Lss", "NPQ_D1","NPQ_D2","NPQ_D3","NPQ_D4","NPQ_D5","NPQ_D6","NPQ_D7","NPQ_D8")

jun1_plate3 <- reshape(jun1_plate3, varying = testmatrix, v.names = c("Fv/Fm", "NPQ"), times = c(1:20), direction = "long")
jun1_plate3 <- jun1_plate3[order(jun1_plate3$Plot, jun1_plate3$Repeat),]
jun1_plate3 <- jun1_plate3[,-7]
rownames(jun1_plate3) <- NULL
