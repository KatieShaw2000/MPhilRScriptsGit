#set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Experimental Work/Drought Experiment")

#get packages ----

library(readxl)
library(ggplot2)
library(ggpubr)
library(AICcmodavg)

#get data ----

data <- read_excel("Isotope_data.xls")
data <- data[,-c(2,3,4,6)]
names(data)[1] <- "Number"

hd <- read_excel("Heading Date.xlsx")
hd <- hd[,-c(5,6,7)]

combined <- merge(hd, data, by = "Number")
combined$`Location Type` <- as.factor(combined$`Location Type`)
combined$Treatment <- as.factor(combined$Treatment)

rm(hd, data)

#get into order for plots ----

order_parms <- within(combined, Genotype <- factor(Genotype, levels=c("B1K-05-12", "B1K-05-08",
                                                                               "B1K-12-10",
                                                                               "B1K-04-03",
                                                                               "B1K-03-17",
                                                                               "B1K-17-17",
                                                                               "B1K-10-01",
                                                                               "B1K-49-10")))

order_basic <- within(combined, Location <- factor(`Location Type`, levels = c("Desert", "Coastal")))

#plot ----

delta_plot1 <- ggplot(order_parms, aes(x=Treatment, y=`d 13C`, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + `Location Type`, ncol=4)+
  theme(legend.position = "none") +
  ylab("delta 13C") + 
  scale_color_manual(values=c("red","blue")) 

delta_plot2 <- ggplot(order_basic, aes(x=Treatment, y=`d 13C`, color = Treatment)) + geom_boxplot() +
  facet_wrap(~Location) + 
  ylab("delta 13C") +
  scale_color_manual(values=c("red","blue"))

ggarrange(delta_plot1, delta_plot2, ncol = 2, labels = c("A", "B"))

delta_interaction <- aov(`d 13C` ~ `Location Type`*Treatment, data = combined)

summary(delta_interaction)

shapiro.test(residuals(lm(`d 13C` ~ `Location Type` * Treatment, data=combined)))

#N content ----
#get SLA so I have N content per unit area ----

SLA <- read.csv("All Parameters/SLA.csv")
SLA <- SLA[,-1]
names(SLA)[1] <- "Number"
SLA <- SLA[,c(1,8)]

combined <- merge(SLA, combined, by = "Number")

combined$N_SLA <- combined$`Amt N%`/combined$SLA
combined$C_SLA <- combined$`Amt C%`/combined$SLA

order_parms <- within(combined, Genotype <- factor(Genotype, levels=c("B1K-05-12", "B1K-05-08",
                                                                      "B1K-12-10",
                                                                      "B1K-04-03",
                                                                      "B1K-03-17",
                                                                      "B1K-17-17",
                                                                      "B1K-10-01",
                                                                      "B1K-49-10")))

order_basic <- within(combined, Location <- factor(`Location Type`, levels = c("Desert", "Coastal")))

N_plot1 <- ggplot(order_parms, aes(x=Treatment, y=N_SLA, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + `Location Type`, ncol=4)+
  theme(legend.position = "none") +
  ylab("%N/SLA") + 
  scale_color_manual(values=c("red","blue")) 

N_plot2 <- ggplot(order_basic, aes(x=Treatment, y=N_SLA, color = Treatment)) + geom_boxplot() +
  facet_wrap(~Location) + 
  ylab("%N/SLA") +
  scale_color_manual(values=c("red","blue"))

ggarrange(N_plot1, N_plot2, ncol = 2, labels = c("A", "B"))

#C content ----
C_plot1 <- ggplot(order_parms, aes(x=Treatment, y=C_SLA, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + `Location Type`, ncol=4)+
  theme(legend.position = "none") +
  ylab("%C/SLA") + 
  scale_color_manual(values=c("red","blue")) 

C_plot2 <- ggplot(order_basic, aes(x=Treatment, y=C_SLA, color = Treatment)) + geom_boxplot() +
  facet_wrap(~Location) + 
  ylab("%C/SLA") +
  scale_color_manual(values=c("red","blue"))

ggarrange(C_plot1, C_plot2, ncol = 2, labels = c("A", "B"))

write.csv(combined,"~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Experimental Work/Drought Experiment/All Parameters/Isotope.csv")
