#set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/GitLink/Final Scripts Output")

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
library(factoextra)
library(corrplot)
library(dplyr)

#get data ----

all_blues <- read_csv("all_blues.csv")
all_blues <- all_blues[,-1]

all_blues <- all_blues[,-c(19,20,21,2,24,25)] #remove NPQ parameters I'm not discussing
all_blues <- all_blues[,-c(18,19)] #remove parameters that strongly correlate

#try PCA and see how aci accessions fit into this ----

for_pca <- all_blues
for_pca$aci <- "No"
for_pca$aci[for_pca$Jmax > 0] <- "Yes" #get yes in column for all aci accessions

for_pca <- for_pca[,-c(7,8,9,10,11,12)] #get rid of the aci parameters
for_pca <- na.omit(for_pca)

names(for_pca)[3] <- "A in High Light"
names(for_pca)[4] <- "gs in High Light"
names(for_pca)[5] <- "A in Low Light"
names(for_pca)[6] <- "gs in Low Light"
names(for_pca)[7] <- "a_induction"
names(for_pca)[8] <- "b_induction"
names(for_pca)[9] <- "a_relaxation"
names(for_pca)[10] <- "b_relaxation"
names(for_pca)[11] <- "c_relaxation"

pca <- prcomp(for_pca[,2:11], scale = TRUE)
summary(pca)

fviz_eig(pca) #scree plot of variances explained by each principal component

fviz_pca_ind(pca, label = "none", col.ind = for_pca$aci, legend.title = "ACi Curve?", palette = c("blue", "red")) 

fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) 
