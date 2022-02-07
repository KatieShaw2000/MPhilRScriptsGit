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

#Set working directory----

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign")

#get data ----

location <- read_excel("shipped_seed_accessions.xlsx")
names(location)[1] <- "Name"
parms <- read_csv("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/BLUPs_for_locations.csv")
combined <- merge(location, parms, by = "Name")

#add in dividing the photosynthetic parameters by SLA ----

combined$Vcmax_SLA <- combined$TPU_Vcmax/combined$SLA_blups
combined$Jmax_SLA <- combined$TPU_Jmax/combined$SLA_blups
combined$asat_SLA <- combined$asat/combined$SLA_blups

asat_SLA <- ggplot(combined, aes(x=Overall, y=asat_SLA, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert", "north" = "North")) + ylab("Asat/SLA") +
  theme(legend.position = "none")

Jmax_SLA <- ggplot(combined, aes(x=Overall, y=Jmax_SLA, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert", "north" = "North")) + ylab("Jmax/SLA") +
  theme(legend.position = "none")

Vcmax_SLA <- ggplot(combined, aes(x=Overall, y=Vcmax_SLA, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert", "north" = "North")) + ylab("Vcmax/SLA") +
  theme(legend.position = "none")

ggarrange(asat_SLA, Jmax_SLA, Vcmax_SLA, ncol=3, nrow=1)

#get some plots ----

iWUE <- ggplot(combined, aes(x=Overall, y=iWUE, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert", "north" = "North")) +
  theme(legend.position = "none")

gs <- ggplot(combined, aes(x=Overall, y=gs, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert", "north" = "North")) + ylab("Stomatal Conductance") +
  theme(legend.position = "none")

sl <- ggplot(combined, aes(x=Overall, y=sl, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert", "north" = "North")) + ylab("Stomatal Limitation") +
  theme(legend.position = "none")

ggarrange(iWUE, gs, sl, ncol=3, nrow=1)

summary(aov(iWUE ~ Overall, data = combined))
summary(aov(gs ~ Overall, data = combined))
summary(aov(sl ~ Overall, data = combined))
TukeyHSD(aov(sl~Overall, data=combined))

asat <- ggplot(combined, aes(x=Overall, y=asat, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert", "north" = "North")) + ylab("Asat") +
  theme(legend.position = "none")

Jmax <- ggplot(combined, aes(x=Overall, y=TPU_Jmax, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert", "north" = "North")) + ylab("Jmax") +
  theme(legend.position = "none")

Vcmax <- ggplot(combined, aes(x=Overall, y=TPU_Vcmax, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert", "north" = "North")) + ylab("Vcmax") +
  theme(legend.position = "none")

ggarrange(asat, Jmax, Vcmax, ncol=3, nrow=1)

summary(aov(asat ~ Overall, data = combined))
summary(aov(TPU_Jmax ~ Overall, data = combined))
summary(aov(TPU_Vcmax ~ Overall, data = combined))

heading_date <- ggplot(combined, aes(x=Overall, y=heading_date_blups, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert", "north" = "North")) + ylab("Heading Date") +
  theme(legend.position = "none")

SLA <- ggplot(combined, aes(x=Overall, y=SLA_blups, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert", "north" = "North")) + ylab("SLA") +
  theme(legend.position = "none")

ggarrange(heading_date, SLA, ncol=2, nrow=1)

summary(aov(heading_date_blups ~ Overall, data = combined))
summary(aov(SLA_blups ~ Overall, data = combined))
TukeyHSD(aov(heading_date_blups~Overall, data=combined))
TukeyHSD(aov(SLA_blups~Overall, data=combined))

a <- ggplot(combined, aes(x=Overall, y=a_blups, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert", "north" = "North")) + ylab("A induction") +
  theme(legend.position = "none")

b <- ggplot(combined, aes(x=Overall, y=b_blups, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert", "north" = "North")) + ylab("B induction") +
  theme(legend.position = "none")

c <- ggplot(combined, aes(x=Overall, y=c_blups, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert", "north" = "North")) + ylab("A relaxation") +
  theme(legend.position = "none")

d <- ggplot(combined, aes(x=Overall, y=d_blups, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert", "north" = "North")) + ylab("B relaxation") +
  theme(legend.position = "none")

e <- ggplot(combined, aes(x=Overall, y=e_blups, fill=Overall)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e desert" = "E Desert", "north" = "North")) + ylab("C relaxation") +
  theme(legend.position = "none")

ggarrange(a,b,c,d,e, ncol=3,nrow=2)

summary(aov(a_blups ~ Overall, data = combined))
summary(aov(b_blups ~ Overall, data = combined))
summary(aov(c_blups ~ Overall, data = combined))
summary(aov(d_blups ~ Overall, data = combined))
summary(aov(e_blups ~ Overall, data = combined))
