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

#get ancestry coefficients data ---- 

`Ancestry_coeff_244accessions_ALStructure_K4 (2)` <- readRDS("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/Ancestry_coeff_244accessions_ALStructure_K4 (2).RDS")
coeff <- `Ancestry_coeff_244accessions_ALStructure_K4 (2)`$Q_hat
coeff <- t(coeff)
coeff <- as.data.frame(coeff)

data <- read_csv("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/BLUPs_for_locations.csv")

#get an average for each site number ----

Names <- rownames(coeff)
coeff <- cbind(Names, coeff)
rownames(coeff) <- NULL

coeff <- coeff[-c(153:155,195:244),]

#change weird name

coeff[54,1] <- "B1K-42-16"
coeff[132,1] <- "B1K-42-10"

coeff$Names <- substr(coeff$Names,1,6)

#see if it's okay to average each site -- look at data frame in ascending order for names
coeff$Names <- as.factor(coeff$Names)
names(coeff)[1] <- "Name"
coeff <- coeff %>% arrange(Name)

coeff$test <- pmax(coeff$North, coeff$Coast, coeff$`Eastern Desert`, coeff$`Southern Desert`)

ggplot(coeff, aes(x=Name, y=test)) + geom_point() + theme(axis.text.x = element_text(angle = 90)) + ylab("Largest Coefficient")
coeff$test2 <- colnames(coeff[,2:5])[max.col(coeff[,2:5])]
ggplot(coeff, aes(x=Name, y=test2)) + geom_point() + theme(axis.text.x = element_text(angle = 90)) + ylab("Location Type")

#think it's fine to do means 
 
means_coeff <- aggregate(coeff[,2:5], list(coeff$Name), mean)
names(means_coeff)[1] <- "Name"

#need to somehow merge these datasets ----
#easiest way i think would be to extract the sites from the names
means_coeff$site <- substr(means_coeff$Name,5,6)
data$site <- substr(data$Name,5,6)

combined <- merge(means_coeff, data, by = "site")
combined <- combined[,-2]
names(combined)[6] <- "Name"
combined <- combined[,-c(9,10,23,24,25,26)]
combined <- na.omit(combined)
combined$Biggest_coeff <- colnames(combined[,2:5])[max.col(combined[,2:5])]
combined <- combined %>% select(Biggest_coeff, everything())

#do some plots ----

ggplot(combined, aes(x=Biggest_coeff, y=SLA_blups)) + geom_boxplot()
ggplot(combined, aes(x=Biggest_coeff, y=heading_date_blups)) + geom_boxplot()
ggplot(combined, aes(x=Biggest_coeff, y=a_blups)) + geom_boxplot()
ggplot(combined, aes(x=Biggest_coeff, y=b_blups)) + geom_boxplot()
ggplot(combined, aes(x=Biggest_coeff, y=c_blups)) + geom_boxplot()
ggplot(combined, aes(x=Biggest_coeff, y=d_blups)) + geom_boxplot()
ggplot(combined, aes(x=Biggest_coeff, y=e_blups)) + geom_boxplot()

ggplot(combined, aes(x=heading_date_blups, color=Biggest_coeff)) + geom_density()
ggplot(combined, aes(x=SLA_blups, color=Biggest_coeff)) + geom_density()
