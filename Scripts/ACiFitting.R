##Get packages

library(readxl)
library(data.table)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(writexl)
library(plantecophys)
library(gridExtra)
library(ggforce)
library(forcats)
library(plyr)
library(lme4)
library(inti)
library(writexl)

#Set working directory 

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files")

#Get data

ACiDATA <- read_excel("GenotypeACiDATA.xlsx")

#Fitting ----

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
fitting <- fitacis(to_fit, "PlotRepeat", fitmethod = "bilinear")

#Plot the fitted curves 

PDFpath1 <- "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files/ACiFitting.pdf"
pdf(file=PDFpath1)
plot(fitting) 
dev.off()

fitting_output <- coef(fitting) #get dataframe with coefficients

#merge coeff with reduced_df ----
with_coef <- merge(reduced_df, fitting_output, by = "PlotRepeat")

#export the merged coeff data table so it's saved somewhere 

write_xlsx(with_coef, "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files/ACiCoef.xlsx")

#Want to plot the ACiDATA to make sure I removed the points I wanted to ----

PDFpath1 <- "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/UpdateRep1.pdf"
pdf(file=PDFpath1)

Repeat1 <- subset(ACiDATA, ACiDATA$Repeat == 1)

for (value in unique(Repeat1$Plot)){
  subset <- subset(Repeat1, Repeat1$Plot == value)
  plot(subset$Ci, subset$A, col='Black', xlim=c(0,1700), ylim=c(-1,80), 
       main=paste("Plot of", value,"_ 1"), xlab="Ci", ylab="A")
}

dev.off()


PDFpath2 <- "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/UpdateRep2.pdf"
pdf(file=PDFpath2)

Repeat2 <- subset(ACiDATA, ACiDATA$Repeat == 2)

for (value in unique(Repeat2$Plot)){
  subset <- subset(Repeat2, Repeat2$Plot == value)
  plot(subset$Ci, subset$A, col='Blue', xlim=c(0,1700), ylim=c(-1,80), 
       main=paste("Plot of", value, "_ 2"), xlab="Ci", ylab="A")
}

dev.off()

#For Repeat 3 

PDFpath3 <- "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/UpdateRep3.pdf"
pdf(file=PDFpath3)

Repeat3 <- subset(ACiDATA, ACiDATA$Repeat == 3)

for (value in unique(Repeat3$Plot)){
  subset <- subset(Repeat3, Repeat3$Plot == value)
  plot(subset$Ci, subset$A, col='Red', xlim=c(0,1700), ylim=c(-1,80), 
       main=paste("Plot of", value, "_ 3"), xlab="Ci", ylab="A")
}

dev.off()







