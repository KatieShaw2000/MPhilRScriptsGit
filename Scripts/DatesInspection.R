#Get packages ----

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

dates <- read_excel("HeadingSamplingDates.xlsx")
dates <- select(dates, -Notes, -Aci)
names(dates)[12] <- "days_post_transplanting"

#Plot days to heading for each genotype ----

ggplot(data = dates, aes(x = factor(0), y = days_post_transplanting, colour = PlotID)) +
  geom_jitter(width = 0.05, size = 2) + 
  xlab("") + 
  stat_summary(fun = mean, geom = "point", shape = 10, size = 2, color = "black") + 
  theme(legend.position = "none") + 
  facet_wrap_paginate( ~ Name, ncol= 8, nrow = 5, page = 1)

#Plotting rep 1 against rep 2 ----

summary(aov(dates$days_post_transplanting ~ Rep, dates)) #ANOVA
dates$Rep <- as.factor(dates$Rep) #Make sure Rep is a factor 

rep1 <- subset(dates, Rep == 1)
rep1 <- select(rep1, Name, days_post_transplanting)
colnames(rep1) <- c("Name", "rep1")
rep2 <- subset(dates, Rep == 2)
rep2 <- select(rep2, Name, days_post_transplanting)
colnames(rep2) <- c("Name", "rep2")

test <- merge(rep1, rep2, by = "Name")

ggplot(test, aes(rep1, rep2))+
  geom_point(alpha = 0.3, colour = "blue")+
  xlab("Rep 1 Time to Heading") +
  ylab("Rep 2 Time to Heading") 

#Boxplot ----

ggplot(dates, aes(x=Rep, y=days_post_transplanting)) + 
  geom_boxplot(notch = TRUE, colour = "tomato3") + 
  geom_jitter(position=position_jitter(0.2), color = "darkolivegreen3") + 
  ylab("Time taken to heading")
  theme_classic()