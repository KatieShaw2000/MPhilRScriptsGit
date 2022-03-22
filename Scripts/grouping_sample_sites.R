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
library(readxl)
#Set working directory 

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign")

#get data ----

# data <- read_csv("Phenotyping Campaign/parms_weather_subset.csv")
# coeff <- `Ancestry_coeff_244accessions_ALStructure_K4 (2)`$Q_hat
# coeff <- t(coeff)
# coeff <- as.data.frame(coeff)

# #get an average for each site number ----
# 
# Names <- rownames(coeff)
# coeff <- cbind(Names, coeff)
# rownames(coeff) <- NULL
# 
# coeff$Names <- substr(coeff$Names,1,6)
# 
# coeff <- coeff[-c(153:155,195:244),]
# 
# means_coeff <- aggregate(coeff[,2:5], list(coeff$Names), mean)
# names(means_coeff)[1] <- "Name"

#load data that uses what I've done so far ----

ancestry_weather_parms <- read_excel("ancestry_weather_parms.xls")

no_sdesert <- ancestry_weather_parms[!(ancestry_weather_parms$Biggest_coeff == "s_desert"),]

#plots ----

ggplot(no_sdesert, aes(x=reorder(Biggest_coeff, iWUE, FUN = median), y=iWUE)) +
  geom_boxplot() + xlab("Location Type")

ggplot(no_sdesert, aes(x=reorder(Biggest_coeff, sl, FUN = median), y=sl)) +
  geom_boxplot() + xlab("Location Type")

ggplot(no_sdesert, aes(x=reorder(Biggest_coeff, gs, FUN = median), y=gs)) +
  geom_boxplot() + xlab("Location Type")

ggplot(no_sdesert, aes(x=reorder(Biggest_coeff, asat, FUN = median), y=asat)) +
  geom_boxplot() + xlab("Location Type")

ggplot(no_sdesert, aes(x=reorder(Biggest_coeff, TPU_Jmax, FUN = median), y=TPU_Jmax)) +
  geom_boxplot() + xlab("Location Type")

ggplot(no_sdesert, aes(x=reorder(Biggest_coeff, TPU_Vcmax, FUN = median), y=TPU_Vcmax)) +
  geom_boxplot() + xlab("Location Type")

ggplot(no_sdesert, aes(x=reorder(Biggest_coeff,heading_date_blups, FUN = median), y=heading_date_blups)) +
  geom_boxplot() + xlab("Location Type")

ggplot(no_sdesert, aes(x=reorder(Biggest_coeff,SLA_blups, FUN = median), y=SLA_blups)) +
  geom_boxplot() + xlab("Location Type")

ggplot(no_sdesert, aes(x=reorder(Biggest_coeff,e_blups, FUN = median), y=e_blups)) +
  geom_boxplot() + xlab("Location Type") + 
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=14),
        axis.title = element_text(size=14))

#get rid of ones we don't have access to in the 270 ----

reduced <- no_sdesert[-c(13,17,21,26,31,37),]

coast <- subset(reduced, Biggest_coeff == "coast")
north <- subset(reduced, Biggest_coeff == "north")
e_desert <- subset(reduced, Biggest_coeff == "e_desert")

seeds <- reduced$Name
seeds <- as.data.frame(seeds)
names(seeds)[1] <- "Genotype"

write_xlsx(seeds, "~/OneDrive - University of Cambridge/MPhil/seed_accessions.xlsx")

#Plots for the reduced no_sdesert accessions ----

#stomatal/water boxplots ----

test <- reduced
levels(test$Biggest_coeff) <- c("Coast", "E Desert", "North")

iWUE <- ggplot(reduced, aes(x=Biggest_coeff, y=iWUE, fill=Biggest_coeff)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                   axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e_desert" = "E Desert", "north" = "North"))+
  theme(legend.position = "none")

gs <- ggplot(reduced, aes(x=Biggest_coeff, y=gs, fill=Biggest_coeff)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e_desert" = "E Desert", "north" = "North")) + ylab("Stomatal Conductance") +
  theme(legend.position = "none")

sl <- ggplot(reduced, aes(x=Biggest_coeff, y=sl, fill=Biggest_coeff)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e_desert" = "E Desert", "north" = "North")) + ylab("Stomatal Limitation") +
  theme(legend.position = "none")

ggarrange(iWUE, gs, sl, ncol=3, nrow=1)

asat <- ggplot(reduced, aes(x=Biggest_coeff, y=asat, fill=Biggest_coeff)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e_desert" = "E Desert", "north" = "North")) + ylab("Asat") +
  theme(legend.position = "none")

Jmax <- ggplot(reduced, aes(x=Biggest_coeff, y=TPU_Jmax, fill=Biggest_coeff)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e_desert" = "E Desert", "north" = "North")) + ylab("Jmax") +
  theme(legend.position = "none")

Vcmax <- ggplot(reduced, aes(x=Biggest_coeff, y=TPU_Vcmax, fill=Biggest_coeff)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e_desert" = "E Desert", "north" = "North")) + ylab("Vcmax") +
  theme(legend.position = "none")

ggarrange(asat, Jmax, Vcmax, ncol=3, nrow=1)

heading_date <- ggplot(reduced, aes(x=Biggest_coeff, y=heading_date_blups, fill=Biggest_coeff)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e_desert" = "E Desert", "north" = "North")) + ylab("Heading Date") +
  theme(legend.position = "none")

SLA <- ggplot(reduced, aes(x=Biggest_coeff, y=SLA_blups, fill=Biggest_coeff)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e_desert" = "E Desert", "north" = "North")) + ylab("SLA") +
  theme(legend.position = "none")

ggarrange(heading_date, SLA, ncol=2, nrow=1)

a <- ggplot(reduced, aes(x=Biggest_coeff, y=a_blups, fill=Biggest_coeff)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e_desert" = "E Desert", "north" = "North")) + ylab("A induction") +
  theme(legend.position = "none")

b <- ggplot(reduced, aes(x=Biggest_coeff, y=b_blups, fill=Biggest_coeff)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e_desert" = "E Desert", "north" = "North")) + ylab("B induction") +
  theme(legend.position = "none")

c <- ggplot(reduced, aes(x=Biggest_coeff, y=c_blups, fill=Biggest_coeff)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e_desert" = "E Desert", "north" = "North")) + ylab("A relaxation") +
  theme(legend.position = "none")

d <- ggplot(reduced, aes(x=Biggest_coeff, y=d_blups, fill=Biggest_coeff)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e_desert" = "E Desert", "north" = "North")) + ylab("B relaxation") +
  theme(legend.position = "none")

e <- ggplot(reduced, aes(x=Biggest_coeff, y=e_blups, fill=Biggest_coeff)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title = element_text(size=14)) + xlab("Location Type") +
  scale_x_discrete(labels = c("coast" = "Coast", "e_desert" = "E Desert", "north" = "North")) + ylab("C relaxation") +
  theme(legend.position = "none")

ggarrange(a,b,c,d,e, ncol=3,nrow=2)

#density plots ----

ggplot(reduced, aes(x=iWUE, color=Biggest_coeff)) + geom_density()

#try some anovas ----

summary(aov(iWUE ~ Biggest_coeff, data = reduced))
summary(aov(sl ~ Biggest_coeff, data = reduced))
summary(aov(gs ~ Biggest_coeff, data = reduced))
summary(aov(asat ~ Biggest_coeff, data = reduced))
summary(aov(TPU_Jmax ~ Biggest_coeff, data = reduced))
summary(aov(TPU_Vcmax ~ Biggest_coeff, data = reduced))
summary(aov(heading_date_blups ~ Biggest_coeff, data = reduced))
TukeyHSD(aov(heading_date_blups~Biggest_coeff, data=reduced))
summary(aov(SLA_blups ~ Biggest_coeff, data = reduced))
TukeyHSD(aov(SLA_blups~Biggest_coeff, data=reduced))
summary(aov(a_blups ~ Biggest_coeff, data = reduced))
summary(aov(b_blups ~ Biggest_coeff, data = reduced))
TukeyHSD(aov(b_blups~Biggest_coeff, data=reduced))
summary(aov(c_blups ~ Biggest_coeff, data = reduced))
summary(aov(d_blups ~ Biggest_coeff, data = reduced))
summary(aov(e_blups ~ Biggest_coeff, data = reduced))
