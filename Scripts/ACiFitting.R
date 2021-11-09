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
library(forcats)
library(plyr)
library(lme4)
library(inti)

#Set working directory 

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files")

#Get data

ACiDATA <- read_excel("GenotypeACiDATA.xlsx")

#Fitting ----

names(ACiDATA)[20] <- "Tleaf"
names(ACiDATA)[64] <- "PPFD"

reduced_df <- select(ACiDATA, Ci, A, Tleaf,PPFD, Plot, Repeat, Rep, Name) #get columns I want
names(reduced_df)[2] <- "Photo"
reduced_df$PlotRepeat <- paste(reduced_df$Plot,reduced_df$Repeat)

reduced_df$Ci <- as.numeric(reduced_df$Ci)
reduced_df$Photo <- as.numeric(reduced_df$Photo)
reduced_df$PPFD <- as.numeric(reduced_df$PPFD)
reduced_df$Tleaf <- as.numeric(reduced_df$Tleaf)

write_xlsx(reduced_df,"~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files/reduced_df.xlsx")

#load as csv- John suggests this works 

to_fit <- read.csv("ACiToFit.csv")
fitting <- fitacis(to_fit, "PlotRepeat")

#Plot the fitted curves 

PDFpath1 <- "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files/ACiFitting.pdf"
pdf(file=PDFpath1)
plot(fitting) 
dev.off()

fitting_output <- coef(fitting) #get dataframe with coefficients

#merge coeff with reduced_df ----
with_coef <- merge(reduced_df, fitting_output, by = "PlotRepeat")

#Plotting!----

with_coef$Rep <- as.factor(with_coef$Rep)
mean_Jmax <- ddply(with_coef, .(Name, Rep), summarise, mean_Jmax = mean(Jmax, na.rm = TRUE)) 
mean_Vcmax <- ddply(with_coef, .(Name, Rep), summarise, mean_Vcmax = mean(Vcmax, na.rm = TRUE)) 

#Plot Jmax against name -- colour being rep

ggplot(data=with_coef, mapping = aes(x=Name, y=Jmax, colour=Rep)) +
  geom_point()

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
  geom_smooth(method = "lm")+
  ggtitle("Rep 1 Jmax vs Rep 2 Jmax")

#Plot Vcmax against name -- colour being rep

ggplot(data=with_coef, mapping = aes(x=Name, y=Vcmax, colour=Rep)) +
  geom_point()

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