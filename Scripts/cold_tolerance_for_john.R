#set working directory ----

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign")

#get libraries ----

library(lme4)
library(desplot)
library(emmeans)
library(ggplot2)
library(readxl)
library(tidyverse)
library(inti)

#get data 

ct <- read_csv("CT_viability.csv")

#change around the file layout ----

ct <- ct[,-c(4,7,8,10)]

#calculating heritability ----

hr <- H2cal(data = ct,
            trait = "CT",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep) + (1|Block) + (1|Column)",
            ran.model = "(1|Name) + (1|Rep) + (1|Block) + (1|Column)")

hr$tabsmr
hr$blups
summary(hr)
summary(hr$model)

ct_blups <- hr$blups
names(ct_blups)[2] <- "ct_blups"

ggplot(ct_blups, aes(x=ct_blups)) + geom_density(color="darkblue", fill="lightblue") + xlab("Cold Tolerance: BLUPs") +
  ylab("Density") + theme_classic()

write.csv(ct_blups, "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/BLUPs for PCA/ct_blups.csv")

#see if heading date and ct blups correlate 

ct_blups <- read_csv("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/BLUPs for PCA/ct_blups.csv")

heading_date_blups <- read_csv("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/BLUPs for PCA/heading_date_blups.csv")

ct_blups <- ct_blups[,-1]
heading_date_blups <- heading_date_blups[,-1]
combined <- merge(ct_blups, heading_date_blups, by = "Name")

ggplot(combined, (aes(x=ct_blups, y=heading_date_blups))) + geom_point()
