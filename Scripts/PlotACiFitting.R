##Get packages

library(readxl)
library(data.table)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(writexl)
library(plantecophys)
library(gridExtra)
library(ggforce)
library(writexl)

#Set working directory 

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files")

#Get data 

aci_coef <- read_excel("ACiCoef.xlsx")
red_aci_coef <- select(aci_coef,Name,Repeat, Rep, Jmax, Vcmax)
red_aci_coef <- red_aci_coef %>% distinct() #remove repeated values

write_xlsx(red_aci_coef, "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/ACiCoefWithLICORs.xlsx")

#Plotting!----

aci_coef$Rep <- as.factor(aci_coef$Rep)
mean_Jmax <- ddply(aci_coef, .(Name, Rep), summarise, mean_Jmax = mean(Jmax, na.rm = TRUE)) 
mean_Vcmax <- ddply(aci_coef, .(Name, Rep), summarise, mean_Vcmax = mean(Vcmax, na.rm = TRUE)) 

#Plot Jmax against name -- colour being rep

ggplot(data = red_aci_coef, aes(x = factor(0), y = Jmax, colour = Repeat)) +
  geom_jitter(width = 0.05, size = 2) + 
  xlab("") + 
  stat_summary(fun.y = mean, geom = "point", shape = 10, size = 2, color = "black") + 
  theme(legend.position = "none") + 
  facet_wrap(~Name)

#Plot means of Jmax from each rep against each other

mean_Jmax_rep1 <- subset(mean_Jmax, Rep == "1")
colnames(mean_Jmax_rep1) <- c("Name", "Rep", "mean_Jmax_rep1")
mean_Jmax_rep1 <- subset(mean_Jmax_rep1, select = -c(Rep))

mean_Jmax_rep2 <- subset(mean_Jmax, Rep == "2")
colnames(mean_Jmax_rep2) <- c("Name", "Rep", "mean_Jmax_rep2")
mean_Jmax_rep2 <- subset(mean_Jmax_rep2, select = -c(Rep))

mean_Jmax <- merge(mean_Jmax_rep1, mean_Jmax_rep2, by = "Name")

ggplot(data=mean_Jmax, mapping = aes(x=mean_Jmax_rep1, y =mean_Jmax_rep2))+
  geom_point()+
  scale_x_continuous(limits = c(100,250), breaks = seq(100,250,25)) + 
  scale_y_continuous(limits = c(100,250), breaks = seq(100,250,25)) + 
  ylab("Jmax (Rep 1)") + 
  xlab("Jmax (Rep 2)") +
  ggtitle("Rep 1 Jmax vs Rep 2 Jmax")

#let's just try to plot the raw values in a boxplot

rep1 <- subset(aci_coef, Rep == "1")
rep2 <- subset(aci_coef, Rep == "2")

ggplot(data=aci_coef, mapping= aes(x=Rep, y=Jmax))+
  geom_boxplot(notch = TRUE)+ 
  geom_point(alpha=0)

ggplot(data=aci_coef, mapping= aes(x=Rep, y=Jmax))+
  geom_boxplot(notch = TRUE)+ 
  geom_point()

#Plot Vcmax against name -- colour being rep

ggplot(data = red_aci_coef, aes(x = factor(0), y = Vcmax, colour = Repeat)) +
  geom_jitter(width = 0.05, size = 2) + 
  xlab("") + 
  stat_summary(fun.y = mean, geom = "point", shape = 10, size = 2, color = "black") + 
  theme(legend.position = "none") + 
  facet_wrap(~Name)

#Plot means of Vcmax from each rep against each other 

mean_Vcmax_rep1 <- subset(mean_Vcmax, Rep == "1")
colnames(mean_Vcmax_rep1) <- c("Name", "Rep", "mean_Vcmax_rep1")
mean_Vcmax_rep1 <- subset(mean_Vcmax_rep1, select = -c(Rep))

mean_Vcmax_rep2 <- subset(mean_Vcmax, Rep == "2")
colnames(mean_Vcmax_rep2) <- c("Name", "Rep", "mean_Vcmax_rep2")
mean_Vcmax_rep2 <- subset(mean_Vcmax_rep2, select = -c(Rep))

mean_Vcmax <- merge(mean_Vcmax_rep1, mean_Vcmax_rep2, by = "Name")

ggplot(data=mean_Vcmax, mapping = aes(x=mean_Vcmax_rep1, y =mean_Vcmax_rep2))+
  geom_point()+
  scale_x_continuous(limits = c(60,130), breaks = seq(60,130,10)) + 
  scale_y_continuous(limits = c(60,130), breaks = seq(60,130,10)) + 
  ylab("Vcmax (Rep 1)") + 
  xlab("Vcmax (Rep 2)") + 
  geom_smooth(method = "lm")+
  ggtitle("Rep 1 Vcmax vs Rep 2 Vcmax")

#Plot jmax against vcmax 

joined <- merge(mean_Jmax, mean_Vcmax, by="Name")

ggplot(joined, aes(mean_Jmax_rep1, mean_Vcmax_rep1))+
  geom_point()

ggplot(joined, aes(mean_Jmax_rep2, mean_Vcmax_rep2)) +
  geom_point()

#Boxplots of Jmax and VCmax for each genotype

ggplot(data=red_aci_coef, aes(x=reorder(Name, Jmax, FUN = median), y=Jmax))+
  geom_point()+
  geom_boxplot()+
  xlab("Genotype")+ ylab("Jmax")+
  scale_y_continuous(limits=c(0,300))+
  theme(axis.text.x = element_text(angle = 90))

ggplot(data=red_aci_coef, aes(x=reorder(Name, Vcmax, FUN = median), y=Vcmax))+
  geom_point()+
  geom_boxplot()+
  xlab("Genotype")+ ylab("Vcmax")+
  theme(axis.text.x = element_text(angle = 90))
