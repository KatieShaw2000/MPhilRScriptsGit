#packages ----

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
library(readxl)
library(dplyr)

#Set working directory----

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign")

#get data ----

location <- read_excel("shipped_seed_accessions.xlsx")
names(location)[1] <- "Name"
parms <- read_csv("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/BLUPs_for_locations.csv")
combined <- merge(location, parms, by = "Name")

#get reduced data frame based on those with highest coefficients for east desert and coast ----

test <- combined[!(combined$Overall == "north"),]
test <- test[order(test$`Average coefficient`),]
test <- test[c(11,12,13,14,15,16,18,19),] #not including ones from same site

#get some plots ----

iWUE <- ggplot(test, aes(x=Overall, y=iWUE, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) +
  theme(legend.position = "none")

gs <- ggplot(test, aes(x=Overall, y=gs, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) + ylab("Stomatal Conductance") +
  theme(legend.position = "none")

sl <- ggplot(test, aes(x=Overall, y=sl, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) + ylab("Stomatal Limitation") +
  theme(legend.position = "none")

ggarrange(iWUE, gs, sl, ncol=3, nrow=1)

summary(aov(iWUE ~ Overall, data = test))
summary(aov(gs ~ Overall, data = test))
summary(aov(sl ~ Overall, data = test))

asat <- ggplot(test, aes(x=Overall, y=asat, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) + ylab("Asat") +
  theme(legend.position = "none")

Jmax <- ggplot(test, aes(x=Overall, y=TPU_Jmax, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) + ylab("Jmax") +
  theme(legend.position = "none")

Vcmax <- ggplot(test, aes(x=Overall, y=TPU_Vcmax, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) + ylab("Vcmax") +
  theme(legend.position = "none")

ggarrange(asat, Jmax, Vcmax, ncol=3, nrow=1)

summary(aov(asat ~ Overall, data = test))
summary(aov(TPU_Jmax ~ Overall, data = test))
summary(aov(TPU_Vcmax ~ Overall, data = test))

heading_date <- ggplot(test, aes(x=Overall, y=heading_date_blups, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) + ylab("Heading Date") +
  theme(legend.position = "none")

SLA <- ggplot(test, aes(x=Overall, y=SLA_blups, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) + ylab("SLA") +
  theme(legend.position = "none")

ggarrange(heading_date, SLA, ncol=2, nrow=1)

summary(aov(heading_date_blups ~ Overall, data = test))
summary(aov(SLA_blups ~ Overall, data = test))

a <- ggplot(test, aes(x=Overall, y=a_blups, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) + ylab("A induction") +
  theme(legend.position = "none")

b <- ggplot(test, aes(x=Overall, y=b_blups, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) + ylab("B induction") +
  theme(legend.position = "none")

c <- ggplot(test, aes(x=Overall, y=c_blups, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) + ylab("A relaxation") +
  theme(legend.position = "none")

d <- ggplot(test, aes(x=Overall, y=d_blups, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) + ylab("B relaxation") +
  theme(legend.position = "none")

e <- ggplot(test, aes(x=Overall, y=e_blups, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert")) + ylab("C relaxation") +
  theme(legend.position = "none")

ggarrange(a,b,c,d,e, ncol=3,nrow=2)

summary(aov(a_blups ~ Overall, data = test))
summary(aov(b_blups ~ Overall, data = test))
summary(aov(c_blups ~ Overall, data = test))
summary(aov(d_blups ~ Overall, data = test))
summary(aov(e_blups ~ Overall, data = test))

