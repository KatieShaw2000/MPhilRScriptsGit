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

#this is simply me double checking everything!!!

#Set working directory 

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign")

#get ancestry coefficients data ---- 

`Ancestry_coeff_244accessions_ALStructure_K4 (2)` <- readRDS("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/Ancestry_coeff_244accessions_ALStructure_K4 (2).RDS")
coeff <- `Ancestry_coeff_244accessions_ALStructure_K4 (2)`$Q_hat
coeff <- t(coeff)
coeff <- as.data.frame(coeff)

data <- read_csv("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/BLUPs_for_locations.csv")

Names <- rownames(coeff)
coeff <- cbind(Names, coeff)
rownames(coeff) <- NULL

coeff <- coeff[-c(153:155,195:244),]

#change weird name

coeff[54,1] <- "B1K-42-16"
coeff[132,1] <- "B1K-42-10"

coeff$Site <- substr(coeff$Names,5,6)
coeff$Names <- as.factor(coeff$Names)
names(coeff)[1] <- "Name"
coeff <- coeff %>% arrange(Site)

coeff$test <- pmax(coeff$North, coeff$Coast, coeff$`Eastern Desert`, coeff$`Southern Desert`)

coeff$test2 <- colnames(coeff[,2:5])[max.col(coeff[,2:5])]

means_coeff <- aggregate(coeff[,2:5], list(coeff$Site), mean)
names(means_coeff)[1] <- "Site"

#have checked this excel sheet with the things above ----

location <- read_excel("shipped_seed_accessions.xlsx")
names(location)[1] <- "Name"
parms <- read_csv("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/BLUPs_for_locations.csv")
combined <- merge(location, parms, by = "Name")

test <- combined[!(combined$Overall == "north"),]
test <- test[order(test$`Average coefficient`),]

#get some plots 

final <- test [c(11,13:19),]

iWUE <- ggplot(final, aes(x=Overall, y=iWUE, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) +
  theme(legend.position = "none")

gs <- ggplot(final, aes(x=Overall, y=gs, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) + ylab("Stomatal Conductance") +
  theme(legend.position = "none")

sl <- ggplot(final, aes(x=Overall, y=sl, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) + ylab("Stomatal Limitation") +
  theme(legend.position = "none")

ggarrange(iWUE, gs, sl, ncol=3, nrow=1)

summary(aov(iWUE ~ Overall, data = test))
summary(aov(gs ~ Overall, data = test))
summary(aov(sl ~ Overall, data = test))

asat <- ggplot(final, aes(x=Overall, y=asat, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) + ylab("Asat") +
  theme(legend.position = "none")

Jmax <- ggplot(final, aes(x=Overall, y=TPU_Jmax, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) + ylab("Jmax") +
  theme(legend.position = "none")

Vcmax <- ggplot(final, aes(x=Overall, y=TPU_Vcmax, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) + ylab("Vcmax") +
  theme(legend.position = "none")

ggarrange(asat, Jmax, Vcmax, ncol=3, nrow=1)

summary(aov(asat ~ Overall, data = test))
summary(aov(TPU_Jmax ~ Overall, data = test))
summary(aov(TPU_Vcmax ~ Overall, data = test))

heading_date <- ggplot(final, aes(x=Overall, y=heading_date_blups, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) + ylab("Heading Date") +
  theme(legend.position = "none")

SLA <- ggplot(final, aes(x=Overall, y=SLA_blups, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) + ylab("SLA") +
  theme(legend.position = "none")

ggarrange(heading_date, SLA, ncol=2, nrow=1)

summary(aov(heading_date_blups ~ Overall, data = test))
summary(aov(SLA_blups ~ Overall, data = test))

write_csv(final, "~/OneDrive - University of Cambridge/MPhil/Experimental Work/Drought_Accessions.csv" )
