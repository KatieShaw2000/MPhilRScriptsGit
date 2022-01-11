#get libraries ----

library(readxl)
library(lme4)
library(desplot)
library(emmeans)
library(ggplot2)
library(readxl)
library(tidyverse)
library(inti)

#get data (and plot means) ----

parms <- read_csv("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/TPUACi.csv")
parms <- parms[,-1]
parms <- separate(parms, PlotRepeat, c("Plot", "Repeat"))
parms <- filter(parms, TPU_Jmax <= 1000)

genotypes <- read_excel("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files/ACiCoefWithLICORs.xlsx")
genotypes <- select(genotypes, Name, Plot)
genotypes <- unique(genotypes)
genotypes <- genotypes[-91,]

parms <- merge(genotypes, parms, by = "Plot")

ggplot(parms, aes(x=reorder(Name, TPU_Vcmax, FUN = median), y=TPU_Vcmax)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Genotype") + ylab("Vcmax (Raw)")
  
ggplot(parms, aes(x=reorder(Name, TPU_Jmax, FUN = median), y=TPU_Jmax)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Genotype") + ylab("Jmax (Raw)")

ggplot(parms, aes(x=TPU_Jmax, y=TPU_Vcmax)) + geom_point()

parms_means <- aggregate(parms[,c(3,4)], list(parms$Plot), mean)
names(parms_means)[1] <- "Plot"

LICOR_ACI <- read_excel("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files/ACiCoefWithLICORs.xlsx")
LICOR_ACI <- select(LICOR_ACI,-Repeat, -Jmax, -Vcmax)
LICOR_ACI <- LICOR_ACI[1:243,]
LICOR_ACI <- LICOR_ACI[,-9]