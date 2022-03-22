#Packages Needed ----

library(readxl)
library(ggplot2)
library(data.table)
library(tidyverse)

#Set working directory ---- 

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files")

#Load Data ----

file.list <- list.files(pattern='*.xlsx', recursive = TRUE)
initialLICOR.list <- lapply(file.list, read_excel)

todrop <- initialLICOR.list[c(1,2,4,5,8,9,12,13,16,17,19,20,22,23,25,26,28,29,33,34,37,38,39)] #need to remove two columns from these
dropped <- map(todrop, ~ (.x %>% select(-6, -10))) #removed appropriate columns 

tokeep <- initialLICOR.list[c(3,6,7,10,11,14,15,18,21,24,27,30,31,32,35,36)] #have 174 columns already

LICOR.list <- append(dropped, tokeep) #now all have 174 columns 

data <- lapply(LICOR.list, tail, -14) #remove the first 14 rows from each element in the list

ACiData <- rbindlist(data)

#Change column names ---- 

names(ACiData)[1] <- 'Sys_obs'
names(ACiData)[2] <- 'Sys_time'
names(ACiData)[3] <- 'Sys_elapsed'
names(ACiData)[4] <- 'Sys_date'
names(ACiData)[5] <- 'Sys_hhmmss'
names(ACiData)[6] <- 'Plot'
names(ACiData)[7] <- 'Repeat'
names(ACiData)[8] <- 'TIME'
names(ACiData)[9] <- 'E'
names(ACiData)[10] <- 'A'
names(ACiData)[11] <- 'Ca'
names(ACiData)[12] <- 'Ci'

#remove unlabelled rows 1989-2002 

#ACiData <- ACiData[-c(1989:2002),]

#change row 1619 NA value to 1157_3 

ACiData[1619,6] <- 1157
ACiData[1619,7] <- 3

#order dataframe

ACiData <- ACiData[order(Plot, Repeat)] #Order dataframe by ascending plot and repeat 

#Make sure the plot names and repeats are numerical data

ACiData$Plot <- as.numeric(ACiData$Plot)
ACiData$Repeat <- as.numeric(ACiData$Repeat)

#Loop to plot all different plots ----

#For Repeat 1 ----

PDFpath1 <- "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files/ACiRep1.pdf"
pdf(file=PDFpath1)

Repeat1 <- subset(ACiData, ACiData$Repeat == 1)

for (value in unique(Repeat1$Plot)){
  subset <- subset(Repeat1, Repeat1$Plot == value)
  plot(subset$Ci, subset$A, col='Black', xlim=c(0,1700), ylim=c(-1,80), 
       main=paste("Plot of", value,"_ 1"), xlab="Ci", ylab="A")
}

dev.off()

#For Repeat 2 ----

PDFpath2 <- "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files/ACiRep2.pdf"
pdf(file=PDFpath2)

Repeat2 <- subset(ACiData, ACiData$Repeat == 2)

for (value in unique(Repeat2$Plot)){
  subset <- subset(Repeat2, Repeat2$Plot == value)
  plot(subset$Ci, subset$A, col='Blue', xlim=c(0,1700), ylim=c(-1,80), 
       main=paste("Plot of", value, "_ 2"), xlab="Ci", ylab="A")
}

dev.off()

#For Repeat 3 ----

PDFpath3 <- "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files/ACiRep3.pdf"
pdf(file=PDFpath3)

Repeat3 <- subset(ACiData, ACiData$Repeat == 3)

for (value in unique(Repeat3$Plot)){
  subset <- subset(Repeat3, Repeat3$Plot == value)
  plot(subset$Ci, subset$A, col='Red', xlim=c(0,1700), ylim=c(-1,80), 
       main=paste("Plot of", value, "_ 3"), xlab="Ci", ylab="A")
}

dev.off()
