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
library(factoextra)
library(corrplot)
library(dplyr)

#get data ----

heading_date <- read.csv("heading_date_blups.csv")
heading_date <- heading_date[,-1]
SLA <- read.csv("SLA_blups.csv")
SLA <- SLA[,-1]
aci <- read.csv("aci_blups.csv")
aci <- aci[,-1]
NPQ <- read.csv("NPQ_blups.csv")
NPQ <- NPQ[,-1]
asat <- read.csv("Asat_blups.csv")
asat <- asat[,-1]

#do a correlation analysis based on NPQ parameters to see which I should remove so as not to bias the dataset ----

cor(NPQ[,2:13], method = "pearson")
corrplot(cor(NPQ[,2:13]), method= "number", type = "upper", diag = FALSE)

pairs(NPQ[,2:13])

plot(NPQ$max_amp_blups, NPQ$gradient_blups) #based on this we have some strong similarities

#remove max_amp_blups, f/g/h blups, end_NPQ blups, gradient blups and c_blups

NPQ <- NPQ[,-c(2,3,4,7,10,11,12)]
corrplot(cor(NPQ[,2:6]), method= "number", type = "upper", diag = FALSE)

#merge to one dataframe for PCA ----

all_blups <- list(heading_date, SLA, aci, NPQ, asat) %>% reduce(left_join, by="Name")

rm(heading_date, NPQ, aci, SLA, asat)

#try pca including heading date ----

pca_data <- na.omit(all_blups)
rownames(pca_data) <- pca_data[,1]
pca_data <- pca_data[,-1]

pca <- prcomp(pca_data, scale = TRUE)
summary(pca)

#try pca excluding heading date ----

pca_data2 <- pca_data[,-1]

pca2 <- prcomp(pca_data2, scale = TRUE)
summary(pca2)

fviz_eig(pca2) #scree plot of variances explained by each principal component

fviz_pca_ind(pca2, 
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("red", "red", "red"),
             repel = TRUE) # Avoid text overlapping

fviz_pca_var(pca2,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)    # Avoid text overlapping

fviz_pca_biplot(pca2, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969") # Individuals color

#get all PCA and see how ACI genotypes fit into this ----

all_pca <- all_blups
all_pca$aci <- "No"
all_pca$aci[all_pca$TPU_Vcmax > 0] <- "Yes"

all_pca <- all_pca[,-c(4,5)]
all_pca <- all_pca[,-2]
rownames(all_pca) <- all_pca[,1]
all_pca <- all_pca[,-1]
all_pca <- na.omit(all_pca)

pca <- prcomp(all_pca[,1:7], scale = TRUE)
summary(pca)

fviz_eig(pca) #scree plot of variances explained by each principal component

fviz_pca_ind(pca, label = "none", col.ind = all_pca$aci, legend.title = "ACi Curve?", palette = c("blue", "red")) 

fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)    # Avoid text overlapping

fviz_pca_biplot(pca2, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969") # Individuals color

