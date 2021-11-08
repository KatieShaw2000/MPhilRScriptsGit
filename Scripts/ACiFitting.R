##Get packages

library(readxl)
library(data.table)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(writexl)
library(plantecophys)

#Set working directory 

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files")

#Get data

ACiDATA <- read_excel("GenotypeACiDATA.xlsx")

#Fitting

names(ACiDATA)[20] <- "Tleaf"
names(ACiDATA)[64] <- "PPFD"

reduced_df <- select(ACiDATA, Ci, A, Tleaf,PPFD, Plot, Repeat, Rep, Name) #get columns I want
names(reduced_df)[2] <- "Photo"
reduced_df$PlotRepeat <- paste(reduced_df$Plot,reduced_df$Repeat)

reduced_df$Ci <- as.numeric(reduced_df$Ci)
reduced_df$Photo <- as.numeric(reduced_df$Photo)
reduced_df$PPFD <- as.numeric(reduced_df$PPFD)
reduced_df$Tleaf <- as.numeric(reduced_df$Tleaf)

write_xlsx(reduced_df,"~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files/reduced_df.xlsx")

#load as csv- John suggests this works 

to_fit <- read.csv("ACiToFit.csv")
fitting <- fitacis(to_fit, "PlotRepeat")
plot(fitting)
fitting_output <- coef(fitting) #get dataframe with coefficients

#merge coeff with reduced_df 
with_coef <- merge(reduced_df, fitting_output, by = "PlotRepeat")


