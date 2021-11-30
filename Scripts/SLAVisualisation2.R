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
  facet_wrap_paginate( ~ Name, ncol= 8, nrow = 5, page = 8)

SLA_genotype_plot

#Get means ----

mean_SLA <- ddply(SLA, .(Name, Rep), summarise, mean_SLA = mean(SLA, na.rm = TRUE)) 

summary(aov(mean_SLA ~ Rep, mean_SLA)) #ANOVA -- no shouldn't be doing an ANOVA!!!!!! Paired t-test I think 

#Stats test to see if the same genotype in rep 2 is lower than rep 1

mean_SLA$Rep <- as.factor(mean_SLA$Rep) #Make sure Rep is a factor 
t.test(mean_SLA ~ Rep, data = mean_SLA, alternative = "two.sided", var.equal = TRUE)

rep1_meanSLA <- subset(mean_SLA, Rep == "1")
rep2_meanSLA <- subset(mean_SLA, Rep == "2")

t.test(rep1_meanSLA$mean_SLA, rep2_meanSLA$mean_SLA, alternative="two.sided", paired=TRUE)

#SLA means boxplot ----

boxplot_raw_means <- ggplot(mean_SLA, aes(x=Rep, y=mean_SLA)) + 
  geom_boxplot(notch = TRUE, colour = "tomato3") + 
  geom_jitter(position=position_jitter(0.2), color = "darkolivegreen3") + 
  scale_y_continuous(limits = c(100,375), breaks = seq(100,375,25)) + 
  theme(text=element_text(size=15))

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
  theme(text=element_text(size=15)) 

scatter_raw_means

grid.arrange(boxplot_raw_means, scatter_raw_means, ncol= 2)

#Plot areas for each genotype ---- 

area_genotype_plot <- ggplot(data = SLA, aes(x = factor(0), y = Area, colour = Plot)) +
  geom_jitter(width = 0.05, size = 2) + 
  xlab("") + 
  stat_summary(fun.y = mean, geom = "point", shape = 10, size = 2, color = "black") + 
  theme(legend.position = "none") + 
  facet_wrap_paginate( ~ Name, ncol= 8, nrow = 5, page = 7)

area_genotype_plot

#Area means boxplot ----

mean_area <- ddply(SLA, .(Name, Rep), summarise, mean_area = mean(Area, na.rm = TRUE)) 
mean_area$Rep <- as.factor(mean_area$Rep) #Make sure Rep is a factor 

ggplot(mean_area, aes(x=Rep, y=mean_area)) + 
  geom_boxplot(notch = TRUE, colour = "tomato3") + 
  geom_jitter(position=position_jitter(0.2), color = "darkolivegreen3") + 
  theme(text=element_text(size=15)) 

#Plot area means of rep1 vs rep2 ----

mean_area_rep1 <- subset(mean_area, Rep == "1")
colnames(mean_area_rep1) <- c("Name", "Rep", "mean_area_rep1")
mean_area_rep1 <- subset(mean_area_rep1, select = -c(Rep))

mean_area_rep2 <- subset(mean_area, Rep == "2")
colnames(mean_area_rep2) <- c("Name", "Rep", "mean_area_rep2")
mean_area_rep2 <- subset(mean_area_rep2, select = -c(Rep))

mean_area_II <- merge(mean_area_rep1, mean_area_rep2, by = "Name")

ggplot(mean_area_II, aes(x = mean_area_rep1, y = mean_area_rep2)) + 
  geom_point(color = "darkolivegreen3") + 
  ylab("Area (Rep 2)") + 
  xlab("Area (Rep 1)") + 
  geom_smooth(method = "lm", colour = "tomato3", fill = "tomato") + 
  theme_classic() 

#Plot mass for each genotype ----

mass_genotype_plot <- ggplot(data = SLA, aes(x = factor(0), y = Mass, colour = Plot)) +
  geom_jitter(width = 0.05, size = 2) + 
  xlab("") + 
  stat_summary(fun.y = mean, geom = "point", shape = 10, size = 2, color = "black") + 
  theme(legend.position = "none") + 
  facet_wrap_paginate( ~ Name, ncol= 8, nrow = 5, page = 7)

mass_genotype_plot

#Mass means boxplot ----

mean_mass <- ddply(SLA, .(Name, Rep), summarise, mean_mass = mean(Mass, na.rm = TRUE)) 
mean_mass$Rep <- as.factor(mean_mass$Rep) #Make sure Rep is a factor 

ggplot(mean_mass, aes(x=Rep, y=mean_mass)) + 
  geom_boxplot(notch = TRUE, colour = "tomato3") + 
  geom_jitter(position=position_jitter(0.2), color = "darkolivegreen3") + 
  theme(text=element_text(size=15)) 

#Plot mass means of rep1 vs rep2 ----

mean_mass_rep1 <- subset(mean_mass, Rep == "1")
colnames(mean_mass_rep1) <- c("Name", "Rep", "mean_mass_rep1")
mean_mass_rep1 <- subset(mean_mass_rep1, select = -c(Rep))

mean_mass_rep2 <- subset(mean_mass, Rep == "2")
colnames(mean_mass_rep2) <- c("Name", "Rep", "mean_mass_rep2")
mean_mass_rep2 <- subset(mean_mass_rep2, select = -c(Rep))

mean_mass_II <- merge(mean_mass_rep1, mean_mass_rep2, by = "Name")

ggplot(mean_mass_II, aes(x = mean_mass_rep1, y = mean_mass_rep2)) + 
  ylab("Mass (Rep 2)") + 
  xlab("Mass (Rep 1)") +  
  scale_x_continuous(limits = c(0.01,0.1), breaks = seq(0.01,0.1,0.01)) + 
  scale_y_continuous(limits = c(0.01,0.1), breaks = seq(0.01,0.1,0.01)) +
  geom_smooth(method = "lm", colour = "tomato3", fill = "tomato") + 
  geom_point(color = "darkolivegreen3") 

#Plot SLA against heading date ----

ggplot(SLA, aes(x=Heading_date, y=SLA, color=Rep)) + geom_point(size=1, shape=4) +
  ggtitle("SLA vs Heading Date")

#Plot mean SLA against mean area ----

#get merged mean dataset
all_means <- merge(mean_SLA_II, mean_area_II, by = "Name")
all_means <- merge(all_means, mean_mass_II, by = "Name")

#for rep 1
ggplot(all_means, aes(mean_SLA_rep1, mean_area_rep1)) +
  geom_point()+
  geom_smooth(method = "lm")

#for rep 2
ggplot(all_means, aes(mean_SLA_rep2, mean_area_rep2)) +
  geom_point()+
  geom_smooth(method = "lm")

#Plot mean SLA against mean mass ----

#for rep 1
ggplot(all_means, aes(mean_SLA_rep1, mean_mass_rep1)) +
  geom_point()+
  geom_smooth(method = "lm")

#for rep 2
ggplot(all_means, aes(mean_SLA_rep2, mean_mass_rep2)) +
  geom_point()+
  geom_smooth(method = "lm")


