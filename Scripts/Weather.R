#packages
library(readxl)
library(ggplot2)
library(tidyr)
library(ggforce)
library(tidyverse)
library(desplot)
library(corrplot)
library(devtools)

#Set working directory 

setwd("~/OneDrive - University of Cambridge/MPhil")

#get data

hubner_weather_data <- read_excel("~/OneDrive - University of Cambridge/MPhil/hubner weather data.xlsx")
GenotypeSite <- read_excel("~/OneDrive - University of Cambridge/MPhil/GenotypeSite.xlsx")

weather_data <- merge(GenotypeSite, hubner_weather_data, by = "Site")
names(weather_data)[2] <- "Name"

#write_csv(weather_data, "~/OneDrive - University of Cambridge/MPhil/genotype_weather.csv")

#get data ----

heading_date <- read.csv("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/BLUPs for PCA/heading_date_blups.csv")
heading_date <- heading_date[,-1]
SLA <- read.csv("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/BLUPs for PCA/SLA_blups.csv")
SLA <- SLA[,-1]
aci <- read.csv("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/BLUPs for PCA/aci_blups.csv")
aci <- aci[,-1]
NPQ <- read.csv("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/BLUPs for PCA/NPQ_blups.csv")
NPQ <- NPQ[,-1]
additional_aci <- read.csv("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/BLUPs for PCA/additional_aci_blups.csv")
additional_aci <- additional_aci[,-1]

#merge all into data frame ----

data <- list(weather_data, heading_date, SLA, aci, NPQ, additional_aci) %>% reduce(left_join, by="Name")

rm(heading_date, NPQ, aci, SLA, additional_aci)
rm(GenotypeSite, hubner_weather_data, weather_data)

#subset data ----

subset <- na.omit(data)

write_csv(subset,"~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/parms_weather_subset.csv")

#try some plotting ----

ggplot(data, aes(x=`MAR (mm)`, y=sl)) + geom_point()
ggplot(data, aes(x=`MAR (mm)`, y=iWUE)) + geom_point()

ggplot(data, aes(x=reorder(Location, sl, FUN = median, na.rm = TRUE), y=sl)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data, aes(x=reorder(Location, iWUE, FUN = median, na.rm = TRUE), y=iWUE)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))

#try and get ancestry coefficients ----










