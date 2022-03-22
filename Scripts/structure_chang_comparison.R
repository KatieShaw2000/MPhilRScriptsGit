#packages
library(readxl)
library(ggplot2)
library(tidyr)
library(ggforce)
library(tidyverse)
library(desplot)
library(corrplot)
library(devtools)
library(writexl)
library(gridExtra)
library(ggpubr)
library(dplyr)

#Set working directory 

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign")

#get ancestry coefficients data from Chang paper ---- 

`Ancestry_coeff_244accessions_ALStructure_K4 (2)` <- readRDS("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/Ancestry_coeff_244accessions_ALStructure_K4 (2).RDS")
chang <- `Ancestry_coeff_244accessions_ALStructure_K4 (2)`$Q_hat
chang <- t(chang)
chang <- as.data.frame(chang)

Genotype <- rownames(chang)
chang <- cbind(Genotype, chang)
rownames(chang) <- NULL

chang <- chang[-c(153:155,195:244),]

chang[54,1] <- "B1K-42-16"
chang[132,1] <- "B1K-42-10"

rm(`Ancestry_coeff_244accessions_ALStructure_K4 (2)`, Genotype)

#get data from STRUCTURE ----

structure <- read_excel("~/OneDrive - University of Cambridge/MPhil/Structure/output_5000_iterations.xlsx")

combined <- merge(chang, structure, by = "Genotype")

#consider the accessions that have been shipped ----

shipped_accessions <- read_excel("shipped_seed_accessions.xlsx")
shipped_accessions <- shipped_accessions[shipped_accessions$Overall != 'north',]

reduced <- shipped_accessions[,c(1,7)]
reduced <- merge(reduced, structure, by = "Genotype")
