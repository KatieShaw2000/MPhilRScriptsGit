#set working directory ----

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/SLA Data")

#get packages needed ----

library(readxl)
library(ggplot2)
library(lme4)
library(desplot)
library(emmeans)
library(tidyverse)
library(inti)

#get data ----

SLA <- read_csv("SLA.csv")

#get plot averages ----

SLA_means <- aggregate(SLA[,7], list(SLA$Name,SLA$Plot,SLA$Rep, SLA$Column, SLA$Block,
                                     SLA$Heading_date, SLA$sampling_date), mean)
names(SLA_means)[1] <- "Genotype"
names(SLA_means)[2] <- "Plot"
names(SLA_means)[3] <- "Rep"
names(SLA_means)[4] <- "Column"
names(SLA_means)[5] <- "Block"
names(SLA_means)[6] <- "Heading_date"
names(SLA_means)[7] <- "Sampling_date"

SLA_means <- SLA_means[order(SLA_means$Genotype),]

SLA_means$Rep <- as.factor(SLA_means$Rep)

#blues----

SLA_models <- H2cal(data = SLA_means,
            trait = "SLA",
            gen.name = "Genotype",
            rep.n = 2,
            fix.model = "0 + Genotype + (1|Rep) + (1|Sampling_date) + (1|Heading_date) + (1|Block) + (1|Column)",
            ran.model = "(1|Genotype) + (1|Rep) + (1|Sampling_date) + (1|Heading_date) + (1|Block) + (1|Column)")

summary(SLA_models$model) #all of the variance is above 0 so leave components in!
SLA_models$tabsmr
SLA_blues <- SLA_models$blues
SLA_blues <- SLA_blues[,c(1,2)]

#export SLA BLUEs ----

write.csv(SLA_blues, "~/OneDrive - University of Cambridge/MPhil/GitLink/Final Scripts Output/SLA_blues.csv")
