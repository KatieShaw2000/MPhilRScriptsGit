#Get packages

library(readxl)
library(data.table)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(writexl)

#Set working directory 

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files")

#Get data

ACiDATA <- read_excel("ACiDATA.xlsx")
ACiDATA$ToDrop <- ACiDATA$ToDrop %>% replace_na('Keep')
ACiDATA <- filter(ACiDATA, ToDrop == "Keep") #Remove dodgy points
ACi <- select(ACiDATA, Plot, Repeat, A, Ci)
ACi$Ci <- as.numeric(ACi$Ci)
ACi$A <- as.numeric(ACi$A)
ACi$Repeat <- as.factor(ACi$Repeat)
ACi$Plot <- as.numeric(ACi$Plot)

#Plot data 

ACi1 <- subset(ACi, Plot <= 2000)
ACi2 <- subset(ACi, Plot >= 2000)
  
plot1000s <- ggplot(data = ACi1, aes(x=Ci, y=A, colour=Repeat)) + 
  geom_point() +
  facet_wrap(facets = vars(Plot))

plot2000s <- ggplot(data = ACi2, aes(x=Ci, y=A, colour=Repeat)) + 
  geom_point() +
  facet_wrap(facets = vars(Plot))

#Plot data based on genotype 

FieldDesign <- read_excel("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/FieldDesign.xlsx")

plot_genotype <- select(FieldDesign, PlotID, Rep, Name)
names(plot_genotype)[1] <- "Plot"
names(plot_genotype)[2] <- "Repeat"

ACi <- merge(ACi, plot_genotype, by = "Plot")
names(ACi)[2] <- "Repeat"
names(ACi)[5] <- "Rep"
ACi$Rep <- as.factor(ACi$Rep)

genotypeplot <- ggplot(data=ACi, aes(x=Ci, y=A, colour=interaction(Rep, Repeat, lex.order=TRUE)))+
  geom_point() +
  scale_colour_manual(values=c("darkturquoise", "deepskyblue", "dodgerblue", "hotpink", "magenta1", "maroon4"))+
  labs(colour="Rep-Repeat") +
  facet_wrap(facets = vars(Name))

genotypeplot

#Export ACiDATA when it is merged 

ACiDATA <- merge(ACiDATA, plot_genotype, by = "Plot")
names(ACiDATA)[7] <- "Repeat"
names(ACiDATA)[177] <- "Rep"

write_xlsx(ACiDATA,"~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files/GenotypeACiDATA.xlsx")

write_xlsx(ACiDATA,"~/OneDrive - University of Cambridge/MPhil/GitLink/Final Scripts Output/ACi_data.xlsx")
