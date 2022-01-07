#Get packages ----

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

#Set working directory ----

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files")

#Get data ----

to_fit <- read.csv("reduced_df.csv")

#fit with wheat km of 10.9 ----

fitting_wheat_km <- fitacis(to_fit, "PlotRepeat", fitmethod = "bilinear", Km = 10.9) #used 10.9 based on Carmo Silva et al., 2010- unsure on units for this?
fitting_output_wheat_km <- coef(fitting_wheat_km)
names(fitting_output_wheat_km)[2] <- "Vcmax_wheat_km"
names(fitting_output_wheat_km)[3] <- "Jmax_wheat_km"
fitting_output_wheat_km <- select(fitting_output_wheat_km, PlotRepeat, Vcmax_wheat_km, Jmax_wheat_km)
fitting_output_wheat_km$Jmax_wheat_km <- as.numeric(fitting_output_wheat_km$Jmax_wheat_km)
#fitting_output_wheat_km <- filter(fitting_output_wheat_km, Jmax_wheat_km <= 2000)
plot(fitting_wheat_km)

PDFpath1 <- "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files/ACiFitting_wheatKm.pdf"
pdf(file=PDFpath1)
plot(fitting_wheat_km) 
dev.off()

#fit with wheat km of 10.9 ----

fitting_wheat_km2 <- fitacis(to_fit, "PlotRepeat", fitmethod = "bilinear", Km = 0.0109) #used 0.0109 based on Carmo Silva et al., 2010- unsure on units for this?
fitting_output_wheat_km2 <- coef(fitting_wheat_km2)
names(fitting_output_wheat_km2)[2] <- "Vcmax_wheat_km2"
names(fitting_output_wheat_km2)[3] <- "Jmax_wheat_km2"
fitting_output_wheat_km2 <- select(fitting_output_wheat_km2, PlotRepeat, Vcmax_wheat_km2, Jmax_wheat_km2)
fitting_output_wheat_km2$Jmax_wheat_km2 <- as.numeric(fitting_output_wheat_km2$Jmax_wheat_km2)
#fitting_output_wheat_km <- filter(fitting_output_wheat_km, Jmax_wheat_km <= 2000)
plot(fitting_wheat_km2)

PDFpath2 <- "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files/ACiFitting_wheatKm2.pdf"
pdf(file=PDFpath2)
plot(fitting_wheat_km2) 
dev.off()

#fit without wheat km ----

fitting <- fitacis(to_fit, "PlotRepeat", fitmethod = "bilinear")
fitting_output <- coef(fitting)
fitting_output <- select(fitting_output, PlotRepeat, Vcmax, Jmax)
plot(fitting)

#compare outputs of wheat km vs not ----
compare <- merge(fitting_output, fitting_output_wheat_km, by = "PlotRepeat")
compare <- merge(compare, fitting_output_wheat_km2, by = "PlotRepeat")

ggplot(compare, aes(x=Vcmax, y=Vcmax_wheat_km)) + geom_point() + ylab("Vcmax Km 10.9") +
  xlim(0,150) + ylim(0,150)

ggplot(compare, aes(x=Vcmax, y=Vcmax_wheat_km2)) + geom_point() + ylab("Vcmax Km 0.0109") +
  xlim(0,150) + ylim(0,150)

ggplot(compare, aes(x=Vcmax_wheat_km, y=Vcmax_wheat_km2)) + geom_point() + ylab("Vcmax Km 0.0109") +
  xlab("Vcmax Km 10.9")+
  xlim(0,100) + ylim(0,100)

ggplot(compare, aes(x=Jmax, y=Jmax_wheat_km)) + geom_point() + ylab("Jmax Km 10.9") +
  xlim(0,400) + ylim(0,400)

ggplot(compare, aes(x=Jmax, y=Jmax_wheat_km2)) + geom_point() + ylab("Jmax Km 0.0109") +
  xlim(0,400) + ylim(0,400)

ggplot(compare, aes(x=Jmax_wheat_km, y=Jmax_wheat_km2)) + geom_point() + ylab("Jmax Km 0.0109") +
  xlab("Jmax Km 10.9")+
  xlim(0,400) + ylim(0,400)

#try to melt dataset to get boxplots quickly ----

compare_melt <- melt(compare)
ggplot(compare_melt, aes(x=variable, y=value)) + geom_boxplot() +
  ylim(0,500) +
  xlab("Variable") + ylab("Value") + 
  scale_x_discrete(labels=c("Vcmax", "Jmax", "Vcmax Km 10.9", "Jmax Km 10.9", "Vcmax Km 0.0109", "Jmax Km 0.0109"))
