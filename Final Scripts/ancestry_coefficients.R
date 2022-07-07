#set working directory ----

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign")

#get packages needed ---- 

library(readxl)

#get data ----

chang_data <- readRDS("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Phenotyping Campaign/Ancestry_coeff_244accessions_ALStructure_K4 (2).RDS")
chang_data <- chang_data$Q_hat
chang_data <- t(chang_data)
chang_data <- as.data.frame(chang_data)
chang_data <- chang_data[-c(153:155,195:244),] #remove ones that aren't B1K panel

Names <- rownames(chang_data) #make row names the first column
chang_data <- cbind(Names, chang_data)
rownames(chang_data) <- NULL

rm(Names)

chang_data[54,1] <- "B1K-42-16"
chang_data[132,1] <- "B1K-42-10"

chang_data <- chang_data[order(chang_data$Names),]
names(chang_data)[1] <- "Genotype"

#get site means for data ----

chang_data$Site <- substr(chang_data$Genotype,5,6)

site_means <- aggregate(chang_data[,2:5], list(chang_data$Site), mean)
names(site_means)[1] <- "Site"
site_means$Site <- as.numeric(site_means$Site)

#export the different datasets so that I can manually set means or direct values for all 320 accessions of field trial ----

write.csv(chang_data,"~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/Chang_ancestry_coefficients.csv")
write.csv(site_means, "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/Chang_site_means.csv")
