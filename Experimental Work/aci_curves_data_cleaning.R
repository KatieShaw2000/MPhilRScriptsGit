#Set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Experimental Work/Drought Experiment/6400s_for_R")

#Get packages ----
library(readxl)
library(ggplot2)
library(data.table)
library(tidyverse)
library(writexl)

#Get data ----

file.list <- list.files(pattern='*.xlsx', recursive = TRUE)
LICOR.list <- lapply(file.list, read_excel)
data <- rbindlist(LICOR.list)
data <- subset(data, !is.na(Pot)) #remove the random non-numerical rows

data <- data[order(Pot)] #order data frame based on pot number

data$Photo <- as.numeric(data$Photo) #get columns to be numerical!
data$Cond <- as.numeric(data$Cond)
data$Ci <- as.numeric(data$Ci)

data$Obs <- rep(1:11, times=83) #easier to remove the first 400ppm point!
data <- subset(data, Obs != 6) #removing the first 400ppm point 

data <- subset(data, Photo >= 0) #removing points below 0
data <- subset(data, Ci >= 0) #removing points below 0

#remove some dodgy points and plots I have two graphs for!

data <- data[-c(27:34),] #removing an additional pot 5 
data <- data[-c(632:639),] #removing an additional pot 112
data <- subset(data, Photo <=50) #removing photo points higher than 50
data <- data[-c(97,106,252,261,298,511,687),] #remove some random dodgy points
data <- data[-c(621),]

#Plot data ----

ggplot(data, aes(x=Ci, y=Photo)) + geom_point() + facet_wrap(facets = vars(Pot))

#Get genotype information too ---- 

glasshouse_info <- read_excel("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Experimental Work/Drought Experiment/glasshouse_info.xlsx")
names(glasshouse_info)[1] <- "Pot"
aci_genotype <- merge(glasshouse_info, data, by = "Pot") #combine 
aci_genotype$Treatment <- as.factor(aci_genotype$Treatment)

#Get plots on a genotype-treatment basis ----

order_location_type <- within(aci_genotype, Genotype <- factor(Genotype, levels=c("B1K-05-12", 
                                                                    "B1K-05-08",
                                                                    "B1K-12-10",
                                                                    "B1K-04-03",
                                                                    "B1K-03-17",
                                                                    "B1K-17-17",
                                                                    "B1K-10-01",
                                                                    "B1K-49-10")))
names(order_location_type)[3] <- "Location"

ggplot(order_location_type, aes(x=Ci, y=Photo, colour=Treatment)) + 
  ylab(bquote('Assimilation ('*mu~'mol'~m^-2~s^-1*')'))+
  xlab("Ci (ppm)")+
  geom_point() + 
  facet_wrap(~Genotype+Location, ncol=4)

write_xlsx(aci_genotype,"~/OneDrive - University of Cambridge/MPhil/Experimental Work/Drought Experiment/cleaned_6400_data.xlsx")
