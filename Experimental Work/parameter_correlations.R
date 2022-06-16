#Set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Experimental Work/Drought Experiment/All Parameters")

#Get packages ----

library(ggplot2)
library(readxl)
library(dplyr)
library(corrplot)
library(ggpubr)

#Get data ----

SLA <- read.csv("SLA.csv")
SLA <- SLA[,-1]
names(SLA)[1] <- "Number"
SLA <- SLA[,c(1:4,8)]

whole_plant <- read.csv("whole_plant.csv")
whole_plant <- whole_plant[,-1]
names(whole_plant)[5] <- "dried_mass"
names(whole_plant)[6] <- "leaf_area"
whole_plant <- whole_plant[,c(1,5,6)]

multispec <- read.csv("multispec.csv")
multispec <- multispec[,-1]
names(multispec)[1] <- "Number"
multispec <- multispec[,c(1,7)]
names(multispec)[2] <- "phipsii"

water_use <- read.csv("water_use.csv")
water_use <- water_use[,-1]
names(water_use)[6] <- "normalised_water_use"
water_use <- water_use[,c(1,5,6)]

aci_parms <- read.csv("aci_parms.csv")
aci_parms <- aci_parms[,-1]
names(aci_parms)[1] <- "Number"
aci_parms <- aci_parms[,c(1, 5:11)]

#merge data ----

all_parms <- merge(SLA, whole_plant, by = "Number")
all_parms <- merge(all_parms, multispec, by = "Number")
all_parms <- merge(all_parms, water_use, by = "Number")                   
all_parms <- merge(all_parms, aci_parms, by = "Number")

names(all_parms)[6] <- "Dried Biomass"
names(all_parms)[7] <- "Leaf Area"
names(all_parms)[8] <- "PhiPSII"
names(all_parms)[9] <- "Total Water Use"
names(all_parms)[10] <- "Normalised Water Use"

all_parms <- all_parms[,c(1:8,11,14,16,17,9,10,12,13,15)]

#correlation plot - do one for 40% and one for 80% for each location type? ----

desert_40 <- subset(all_parms, Location == "Desert" & Treatment == "40")
desert_40 <- na.omit(desert_40)

desert_80 <- subset(all_parms, Location == "Desert" & Treatment == "80") 
desert_80 <- na.omit(desert_80)

coast_40 <- subset(all_parms, Location == "Coastal" & Treatment == "40") 

coast_80 <- subset(all_parms, Location == "Coastal" & Treatment == "80")

corrplot(cor(desert_40[,5:17]), type = "upper", diag = FALSE, method = "color",)
corrplot(cor(desert_80[,5:17]), type = "upper", diag = FALSE, method = "color")
corrplot(cor(coast_40[,5:17]), type = "upper", diag = FALSE, method = "color")
corrplot(cor(coast_80[,5:17]), type = "upper", diag = FALSE, method = "color")


          