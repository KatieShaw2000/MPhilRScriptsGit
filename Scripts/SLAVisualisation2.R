#Packages needed ----

library(ggplot2)
library(gridExtra)
library(tidyr)
library(ggforce)
library(forcats)
library(plyr)
library(lme4)
library(inti)
library(readxl)


#Set working directory ---- 

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign")


#Get data ----

SLA <- read_excel("SLA Data/SLA.xlsx")
SLA = data.frame(SLA)
SLA$Rep <- as.factor(SLA$Rep)
SLA$Plot <- as.factor(SLA$Plot)

#Plot all SLA for each genotype ----

SLA_genotype_plot <- ggplot(data = SLA, aes(x = factor(0), y = SLA, colour = Plot)) +
  geom_jitter(width = 0.05, size = 2) + 
  xlab("") + 
  stat_summary(fun.y = mean, geom = "point", shape = 10, size = 2, color = "black") + 
  theme(legend.position = "none") + 
  facet_wrap_paginate( ~ Name, ncol= 8, nrow = 5, page = 7)

SLA_genotype_plot

#Get means ----

mean_SLA <- ddply(SLA, .(Name, Rep), summarise, mean_SLA = mean(SLA, na.rm = TRUE)) 

summary(aov(mean_SLA ~ Rep, mean_SLA)) #ANOVA

mean_SLA$Rep <- as.factor(mean_SLA$Rep) #Make sure Rep is a factor 

#SLA means boxplot ----

boxplot_raw_means <- ggplot(mean_SLA, aes(x=Rep, y=mean_SLA)) + 
  geom_boxplot(notch = TRUE, colour = "tomato3") + 
  geom_jitter(position=position_jitter(0.2), color = "darkolivegreen3") + 
  scale_y_continuous(limits = c(100,375), breaks = seq(100,375,25)) + 
  theme_classic()

boxplot_raw_means


#Plotting SLA means of rep1 vs rep2 ----

mean_SLA_rep1 <- subset(mean_SLA, Rep == "1")
colnames(mean_SLA_rep1) <- c("Name", "Rep", "mean_SLA_rep1")
mean_SLA_rep1 <- subset(mean_SLA_rep1, select = -c(Rep))

mean_SLA_rep2 <- subset(mean_SLA, Rep == "2")
colnames(mean_SLA_rep2) <- c("Name", "Rep", "mean_SLA_rep2")
mean_SLA_rep2 <- subset(mean_SLA_rep2, select = -c(Rep))

mean_SLA_II <- merge(mean_SLA_rep1, mean_SLA_rep2, by = "Name")

cor.test(mean_SLA_II$mean_SLA_rep1, mean_SLA_II$mean_SLA_rep2, method = "pearson")

scatter_raw_means <- ggplot(mean_SLA_II, aes(x = mean_SLA_rep1, y = mean_SLA_rep2)) + 
  geom_point(color = "darkolivegreen3") + 
  scale_x_continuous(limits = c(100,375), breaks = seq(100,375,25)) + 
  scale_y_continuous(limits = c(100,375), breaks = seq(100,375,25)) + 
  ylab("SLA (Rep 2)") + 
  xlab("SLA (Rep 1)") + 
  geom_smooth(method = "lm", colour = "tomato3", fill = "tomato") + 
  theme_classic() 

scatter_raw_means

grid.arrange(boxplot_raw_means, scatter_raw_means, ncol= 2)

#Plot areas for each genotype ---- 
#Area means boxplot ----
#Plot area means of rep1 vs rep2 ----
#Plot mass for each genotype ----
#Mass means boxplot ----
#Plot mass means of rep1 vs rep2 ----
#Plot SLA against heading date ----
#Plot mean SLA against mean area ----
#Plot mean SLA against mean mass ----