#set working directory ----

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/BLUPs for PCA")

#get libraries ----

library(lme4)
library(desplot)
library(emmeans)
library(ggplot2)
library(readxl)
library(tidyverse)
library(inti)
library(devtools)
library(ggbiplot)

#get data ----

heading_date <- read.csv("heading_date_blups.csv")
heading_date <- heading_date[,-1]
SLA <- read.csv("SLA_blups.csv")
SLA <- SLA[,-1]
aci <- read.csv("aci_blups.csv")
aci <- aci[,-1]
NPQ <- read.csv("NPQ_blups.csv")
NPQ <- NPQ[,-1]

#merge to one dataframe for PCA ----

all_blups <- list(heading_date, SLA, aci, NPQ) %>% reduce(left_join, by="Name")

all_blups_restricted <- na.omit(all_blups)
rownames(all_blups_restricted) <- all_blups_restricted[,1]
all_blups_restricted <- all_blups_restricted[,-1]

without_aci_blups <- all_blups[,-c(4,5)]
rownames(without_aci_blups) <- without_aci_blups[,1]
without_aci_blups <- without_aci_blups[,-1]
without_aci_blups <- na.omit(without_aci_blups)

#try a PCA ----

pca <- prcomp(all_blups_restricted[,c(1:16)], center = TRUE, scale. = TRUE)
summary(pca)
str(pca)
ggbiplot(pca)
ggbiplot(pca, labels = rownames(all_blups_restricted))

pca2 <- prcomp(without_aci_blups, center = TRUE, scale. = TRUE)
summary(pca2) 
ggbiplot(pca2)
ggbiplot(pca2, labels = rownames(without_aci_blups))

#try and group based on site? ---- 
GenotypeSite <- read_excel("~/OneDrive - University of Cambridge/MPhil/GenotypeSite.xlsx")

names(GenotypeSite)[1] <- "Name"
site_blups <- merge(GenotypeSite, all_blups, by ="Name")

site_without_aci_blups <- site_blups[,-c(5,6)]
site_without_aci_blups <- na.omit(site_without_aci_blups)
site_without_aci_blups$Site <- as.factor(site_without_aci_blups$Site)

pca3 <- prcomp(site_without_aci_blups[,c(3:16)], center = TRUE, scale. = TRUE)
summary(pca3)
ggbiplot(pca3, groups = site_without_aci_blups$Site)
