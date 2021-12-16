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

#fit with wheat km ----

fitting_wheat_km <- fitacis(to_fit, "PlotRepeat", fitmethod = "bilinear", Km = 10.9) #used 10.9 based on Carmo Silva et al., 2010
fitting_output_wheat_km <- coef(fitting_wheat_km)
names(fitting_output_wheat_km)[2] <- "Vcmax_wheat_km"
names(fitting_output_wheat_km)[3] <- "Jmax_wheat_km"
fitting_output_wheat_km <- select(fitting_output_wheat_km, PlotRepeat, Vcmax_wheat_km, Jmax_wheat_km)
plot(fitting_wheat_km)

PDFpath1 <- "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files/ACiFitting_wheatKm.pdf"
pdf(file=PDFpath1)
plot(fitting_wheat_km) 
dev.off()

#fit without wheat km ----

fitting <- fitacis(to_fit, "PlotRepeat", fitmethod = "bilinear")
fitting_output <- coef(fitting)
fitting_output <- select(fitting_output, PlotRepeat, Vcmax, Jmax)
plot(fitting)

#compare outputs of wheat km vs not ----
compare <- merge(fitting_output, fitting_output_wheat_km, by = "PlotRepeat")
plot(fitting_output$Vcmax, fitting_output_wheat_km$Vcmax_wheat_km)
plot(fitting_output$Jmax, fitting_output_wheat_km$Jmax_wheat_km)
