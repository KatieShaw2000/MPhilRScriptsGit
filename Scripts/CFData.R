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


testmatrix <- matrix(c("Fv/Fm_L1","Fv/Fm_L2","Fv/Fm_L3","Fv/Fm_L4","Fv/Fm_L5","Fv/Fm_L6","Fv/Fm_L7","Fv/Fm_L8","Fv/Fm_L9","Fv/Fm_L10","Fv/Fm_L11",
                       "Fv/Fm_Lss", "Fv/Fm_D1","Fv/Fm_D2","Fv/Fm_D3","Fv/Fm_D4","Fv/Fm_D5","Fv/Fm_D6","Fv/Fm_D7","Fv/Fm_D8",
                       "NPQ_L1","NPQ_L2","NPQ_L3","NPQ_L4","NPQ_L5","NPQ_L6","NPQ_L7","NPQ_L8","NPQ_L9","NPQ_L10","NPQ_L11", 
                       "NPQ_Lss", "NPQ_D1","NPQ_D2","NPQ_D3","NPQ_D4","NPQ_D5","NPQ_D6","NPQ_D7","NPQ_D8"), nrow=2)

plates <- c("Plot", "Repeat", "QY_max", "time", "Fv/Fm", "NPQ", "Filename")

for (file in files) {
  plate <- read.csv(file, skip=2)
  plate <- t(plate)
  plate <- data.frame(plate)
  rownames(plate) <- NULL
  colnames(plate) <- plate[1,]
  plate <- plate[-1,]
  plate <- select(plate,"Plot", "Repeat", "QY_max", 
                  "Fv/Fm_L1","Fv/Fm_L2","Fv/Fm_L3","Fv/Fm_L4","Fv/Fm_L5","Fv/Fm_L6","Fv/Fm_L7","Fv/Fm_L8","Fv/Fm_L9","Fv/Fm_L10","Fv/Fm_L11",
                  "Fv/Fm_Lss", "Fv/Fm_D1","Fv/Fm_D2","Fv/Fm_D3","Fv/Fm_D4","Fv/Fm_D5","Fv/Fm_D6","Fv/Fm_D7","Fv/Fm_D8",
                  "NPQ_L1","NPQ_L2","NPQ_L3","NPQ_L4","NPQ_L5","NPQ_L6","NPQ_L7","NPQ_L8","NPQ_L9","NPQ_L10","NPQ_L11", 
                  "NPQ_Lss", "NPQ_D1","NPQ_D2","NPQ_D3","NPQ_D4","NPQ_D5","NPQ_D6","NPQ_D7","NPQ_D8")
  plate <- reshape(plate, varying = testmatrix, v.names = c("Fv/Fm", "NPQ"), times = c(1:20), direction = "long")
  plate <- plate[order(plate$Plot, plate$Repeat),]
  plate <- plate[,-7]
  rownames(plate) <- NULL
  plate$Filename <- rep(c(file), times=20)
  plates <- rbind(plates, plate)
}

output <- plates[-1,]
output <- output[order(output$Plot, output$Repeat),]

output$QY_max <- as.numeric(output$QY_max)
output$`Fv/Fm` <- as.numeric(output$`Fv/Fm`)
output$NPQ <- as.numeric(output$NPQ)
output$time <- as.numeric(output$time)
output$Repeat <- as.factor(output$Repeat)

time_post_light_on <- c(20,40,60,120,180,240,300,360,420,480,540,600,NA,NA,NA,NA,NA,NA,NA,NA)
time_post_light_off <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,20,40,60,120,180,360,540,720)
cumulative_time <- c(20,40,60,120,180,240,300,360,420,480,540,600,620,640,660,720,780,960,1140,1320)

output$time_post_light_on <- rep(time_post_light_on, times=1548)
output$time_post_light_off <- rep(time_post_light_off, times=1548)
output$cumulative_time <- rep(cumulative_time, times=1548)

output <- output[,-4]

#visualise ----

ggplot(data=output, aes(x=cumulative_time, y=NPQ, colour=Repeat)) +
  geom_point()+
  facet_wrap_paginate( ~ Plot, ncol= 10, nrow = 5, page = 1)

ggplot(data=output, aes(x=cumulative_time, y=`Fv/Fm`, colour=Repeat)) +
  geom_point()+
  facet_wrap_paginate( ~ Plot, ncol= 10, nrow = 5, page = 1)


#jun 1 ----

jun1_plate1 <- read.csv("June 1/plate1_june1.csv", skip =2, fill = TRUE)
jun1_plate1 <- t(jun1_plate1) #transpose 
jun1_plate1 <- data.frame(jun1_plate1) #make dataframe
rownames(jun1_plate1) <- NULL
colnames(jun1_plate1) <- jun1_plate1[1,]
jun1_plate1 <- jun1_plate1[-1,]
jun1_plate1$Plot <- c(1107, 1192, 2006, 2159, 2307, 1107, 2020, 2020, 1066, 1066, 1202, 1080, 2159, 1163, 1117, 2166,
                         1117, 1227, 1010, 2107, 1194, 1117, 1320, 2159, 2072, 1033, 1010, 1108, 1227, 1029)
jun1_plate1$Repeat <- c(1,1,1,1,1,2,3,2,2,3,1,1,3,1,3,2,2,1,2,3,2,1,3,2,3,1,3,2,2,2)

jun1_plate1 <- select(jun1_plate1, "Plot", "Repeat", "QY_max", 
                      "Fv/Fm_L1","Fv/Fm_L2","Fv/Fm_L3","Fv/Fm_L4","Fv/Fm_L5","Fv/Fm_L6","Fv/Fm_L7","Fv/Fm_L8","Fv/Fm_L9","Fv/Fm_L10","Fv/Fm_L11",
                      "Fv/Fm_Lss", "Fv/Fm_D1","Fv/Fm_D2","Fv/Fm_D3","Fv/Fm_D4","Fv/Fm_D5","Fv/Fm_D6","Fv/Fm_D7","Fv/Fm_D8",
                      "NPQ_L1","NPQ_L2","NPQ_L3","NPQ_L4","NPQ_L5","NPQ_L6","NPQ_L7","NPQ_L8","NPQ_L9","NPQ_L10","NPQ_L11", 
                      "NPQ_Lss", "NPQ_D1","NPQ_D2","NPQ_D3","NPQ_D4","NPQ_D5","NPQ_D6","NPQ_D7","NPQ_D8")

testmatrix <- matrix(c("Fv/Fm_L1","Fv/Fm_L2","Fv/Fm_L3","Fv/Fm_L4","Fv/Fm_L5","Fv/Fm_L6","Fv/Fm_L7","Fv/Fm_L8","Fv/Fm_L9","Fv/Fm_L10","Fv/Fm_L11",
                       "Fv/Fm_Lss", "Fv/Fm_D1","Fv/Fm_D2","Fv/Fm_D3","Fv/Fm_D4","Fv/Fm_D5","Fv/Fm_D6","Fv/Fm_D7","Fv/Fm_D8",
                       "NPQ_L1","NPQ_L2","NPQ_L3","NPQ_L4","NPQ_L5","NPQ_L6","NPQ_L7","NPQ_L8","NPQ_L9","NPQ_L10","NPQ_L11", 
                       "NPQ_Lss", "NPQ_D1","NPQ_D2","NPQ_D3","NPQ_D4","NPQ_D5","NPQ_D6","NPQ_D7","NPQ_D8"), nrow=2)

test <- reshape(jun1_plate1, varying = testmatrix, v.names = c("Fv/Fm", "NPQ"), times = c(1:20), direction = "long")
test <- test[order(test$Plot, test$Repeat),]
test <- test[,-7]

jun1_plate1 <- test
rownames(jun1_plate1) <- NULL

###

jun1_plate2 <- read.delim("June 1/Plate 2.txt", skip =2, fill = TRUE)
jun1_plate2 <- t(jun1_plate2) #transpose 
jun1_plate2 <- data.frame(jun1_plate2) #make dataframe
rownames(jun1_plate2) <- NULL
colnames(jun1_plate2) <- jun1_plate2[1,]
jun1_plate2 <- jun1_plate2[-1,]

jun1_plate2$Plot <- c(2307,2242,1258,1033,2242,1080,1010,1281,2091,1098,2166,1194,1026,1192,1258,1202,
                      1026,1163,2006,1080,1258,1025,1320,1029,2006,1098,1133,1029,1133,1281)
jun1_plate2$Repeat <- c(2,2,1,2,3,2,1,1,3,2,3,3,1,2,2,2,2,2,2,3,3,3,2,3,3,3,1,1,2,3)

jun1_plate2 <- select(jun1_plate2, "Plot", "Repeat", "QY_max", 
                      "Fv/Fm_L1","Fv/Fm_L2","Fv/Fm_L3","Fv/Fm_L4","Fv/Fm_L5","Fv/Fm_L6","Fv/Fm_L7","Fv/Fm_L8","Fv/Fm_L9","Fv/Fm_L10","Fv/Fm_L11",
                      "Fv/Fm_Lss", "Fv/Fm_D1","Fv/Fm_D2","Fv/Fm_D3","Fv/Fm_D4","Fv/Fm_D5","Fv/Fm_D6","Fv/Fm_D7","Fv/Fm_D8",
                      "NPQ_L1","NPQ_L2","NPQ_L3","NPQ_L4","NPQ_L5","NPQ_L6","NPQ_L7","NPQ_L8","NPQ_L9","NPQ_L10","NPQ_L11", 
                      "NPQ_Lss", "NPQ_D1","NPQ_D2","NPQ_D3","NPQ_D4","NPQ_D5","NPQ_D6","NPQ_D7","NPQ_D8")

jun1_plate2 <- reshape(jun1_plate2, varying = testmatrix, v.names = c("Fv/Fm", "NPQ"), times = c(1:20), direction = "long")
jun1_plate2 <- jun1_plate2[order(jun1_plate2$Plot, jun1_plate2$Repeat),]
jun1_plate2 <- jun1_plate2[,-7]
rownames(jun1_plate2) <- NULL

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

###

jun1_plate4 <- read.delim("June 1/Plate 4.TXT", skip =2, fill = TRUE)
jun1_plate4 <- t(jun1_plate4) #transpose 
jun1_plate4 <- data.frame(jun1_plate4) #make dataframe
rownames(jun1_plate4) <- NULL
colnames(jun1_plate4) <- jun1_plate4[1,]
jun1_plate4 <- jun1_plate4[-1,]

jun1_plate4$Plot <- c(1108,2020,1025,2183,2072,2072)
jun1_plate4$Repeat <- c(3,1,2,2,2,1)

jun1_plate4 <- select(jun1_plate4, "Plot", "Repeat", "QY_max", 
                      "Fv/Fm_L1","Fv/Fm_L2","Fv/Fm_L3","Fv/Fm_L4","Fv/Fm_L5","Fv/Fm_L6","Fv/Fm_L7","Fv/Fm_L8","Fv/Fm_L9","Fv/Fm_L10","Fv/Fm_L11",
                      "Fv/Fm_Lss", "Fv/Fm_D1","Fv/Fm_D2","Fv/Fm_D3","Fv/Fm_D4","Fv/Fm_D5","Fv/Fm_D6","Fv/Fm_D7","Fv/Fm_D8",
                      "NPQ_L1","NPQ_L2","NPQ_L3","NPQ_L4","NPQ_L5","NPQ_L6","NPQ_L7","NPQ_L8","NPQ_L9","NPQ_L10","NPQ_L11", 
                      "NPQ_Lss", "NPQ_D1","NPQ_D2","NPQ_D3","NPQ_D4","NPQ_D5","NPQ_D6","NPQ_D7","NPQ_D8")

jun1_plate4 <- reshape(jun1_plate4, varying = testmatrix, v.names = c("Fv/Fm", "NPQ"), times = c(1:20), direction = "long")
jun1_plate4 <- jun1_plate4[order(jun1_plate4$Plot, jun1_plate4$Repeat),]
jun1_plate4 <- jun1_plate4[,-7]
rownames(jun1_plate4) <- NULL


jun1 <- rbind(jun1_plate1, jun1_plate2, jun1_plate3, jun1_plate4)
jun1$Repeat <- as.factor(jun1$Repeat)
jun1$`Fv/Fm` <- as.numeric(jun1$`Fv/Fm`)
jun1$NPQ <- as.numeric(jun1$NPQ)
jun1$QY_max <- as.numeric(jun1$QY_max)
