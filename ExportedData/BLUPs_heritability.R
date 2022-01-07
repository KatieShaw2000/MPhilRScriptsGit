#Get packages ----

library(readxl)
library(lme4)
library(ggplot2)
library(tidyverse)
library(lubridate)

#Get SLA data ----

SLA <- read_csv("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/SLA Data/SLA.csv")

#Model for SLA ----

SLA_model <- lmer(data=SLA, SLA ~ (1|Name) + (1|Rep) + (1|Column:Rep) + (1|sampling_date) + (1|heading_to_sampling))
summary(SLA_model)

var_SLA <- as.data.frame(VarCorr(SLA_model, comp = "vcov"))
head(var_SLA)

h_SLA <- var_SLA[1,4]/sum(var_SLA[,4]) #heritability calculation

ggplot(var_SLA, aes(x=grp, y=vcov)) +geom_bar(stat="identity")

#Calculating BLUPs ----

SLA_blups <- ranef(SLA_model)
SLA_blups_genotypes <- SLA_blups$Name
SLA_blups_genotypes$Name <- rownames(SLA_blups_genotypes)
rownames(SLA_blups_genotypes) <- NULL
colnames(SLA_blups_genotypes) <- c("SLA", "Line")
SLA_blups_genotypes$adjusted_SLA <- SLA_blups_genotypes$SLA + mean(SLA$SLA, na.rm = TRUE)
