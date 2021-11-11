#Get packages ----

library(readxl)
library(lme4)
library(ggplot2)
library(tidyverse)
library(lubridate)

#Model for SLA ----

#Load data

SLA <- read_excel("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/SLA Data/SLA.xlsx")
SLA <- filter(SLA, ToDrop == "Keep") #filter out outliers I picked manually 
SLA <- select(SLA, -ToDrop)

dates <- read_excel("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/HeadingSamplingDates.xlsx")

dates <- select(dates, Name, Rep, Column, "Heading-Sampling", "Transplanting-Heading", Sampling)

colnames(dates) <- c("Name", "Rep", "Column", "heading_to_sampling",
                     "transplanting_to_heading", "sampling_date")

mergeSLA <- merge(SLA, dates, by = c("Name", "Rep"))

#Model for SLA

SLA_model <- lmer(data=mergeSLA, SLA ~ (1|Name) + (1|Rep) + (1|Column:Rep) + (1|sampling_date) + (1|heading_to_sampling))
summary(SLA_model)

var_SLA <- as.data.frame(VarCorr(SLA_model, comp = "vcov"))
head(var_SLA)

h_SLA <- var_SLA[1,4]/sum(var_SLA[,4])

#Calculating BLUPs

SLA_blups <- ranef(SLA_model)
SLA_blups_genotypes <- SLA_blups$Name
SLA_blups_genotypes$Name <- rownames(SLA_blups_genotypes)
rownames(SLA_blups_genotypes) <- NULL
colnames(SLA_blups_genotypes) <- c("SLA", "Line")
SLA_blups_genotypes$adjusted_SLA <- SLA_blups_genotypes$SLA + mean(SLA$SLA, na.rm = TRUE)

#Density plot 

ggplot(SLA_blups_genotypes, aes(x=adjusted_SLA)) +
  geom_density()+ 
  scale_x_continuous(limits=c(190,270), breaks = seq(190,270,10))+
  xlab("SLA")+
  ylab("Density")

#Boxplot

mergeSLA$sampling_date <- as.factor(mergeSLA$sampling_date)
ggplot(mergeSLA, aes(sampling_date, SLA))+
  geom_point()+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))

#Export 

write.csv(SLA_blups_genotypes,"~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/SLA_blups.csv", row.names = F)

#Model for Heading Date ----

#it wants numeric data so use the number of days between transplanting and heading 

hd_model <- lmer(data=mergeSLA, transplanting_to_heading ~ (1|Name) + (1|Rep) + (1|Column:Rep))
summary(hd_model)

var_hd <- as.data.frame(VarCorr(hd_model, comp = "vcov"))
head(var_hd)

h_hd <- var_hd[1,4]/sum(var_hd[,4])

#Calculating BLUPs

hd_blups <- ranef(hd_model)
hd_blups_genotypes <- hd_blups$Name
hd_blups_genotypes$Name <- rownames(hd_blups_genotypes)
rownames(hd_blups_genotypes) <- NULL
colnames(hd_blups_genotypes) <- c("Days_to_Heading", "Line")
hd_blups_genotypes$adjusted_hd <- hd_blups_genotypes$Days_to_Heading + mean(mergeSLA$transplanting_to_heading, na.rm = TRUE)

#Density plot 

ggplot(hd_blups_genotypes, aes(x=adjusted_hd)) +
  geom_density()+ 
  scale_x_continuous(limits=c(24,40), breaks = seq(24,40,2))+
  xlab("Days to Heading")+
  ylab("Density")

#Export 

write.csv(hd_blups_genotypes,"~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/hd_blups.csv", row.names = F)

#Model for Jmax ----

#Load data 

LICOR_ACI <- read_excel("OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/ACiCoefWithLICORs.xlsx")
LICOR_ACI <- LICOR_ACI[1:243,]
names(LICOR_ACI)[9] <- "LICOR_time"
LICOR_ACI$LICOR_time <- hour(LICOR_ACI$LICOR_time) #get time in hour 

#need to get column into data frame

column_plot_rep <- select(mergeSLA, Plot, Rep, Repeat, Column)

LICOR_ACI <- merge(LICOR_ACI, column_plot_rep, by = c("Plot", "Rep", "Repeat"))

Jmax_model <-  lmer(data=LICOR_ACI, Jmax ~ (1|Name) + (1|Rep) + (1|Rep:Column) + (1|LICOR_user) + 
                      (1|LICOR_time) + (1|LICOR_date) + (1|LICOR_ID))

summary(Jmax_model)

var_Jmax <- as.data.frame(VarCorr(Jmax_model, comp = "vcov"))
head(var_Jmax)

h_Jmax <- var_Jmax[1,4]/sum(var_Jmax[,4])

#sort out BLUPS 

Jmax_blups <- ranef(Jmax_model)
Jmax_blups_genotypes <- Jmax_blups$Name
Jmax_blups_genotypes$Name <- rownames(Jmax_blups_genotypes)
rownames(Jmax_blups_genotypes) <- NULL
colnames(Jmax_blups_genotypes) <- c("Jmax", "Line")
Jmax_blups_genotypes$adjusted_Jmax <- Jmax_blups_genotypes$Jmax + mean(LICOR_ACI$Jmax, na.rm = TRUE)

#Density plot 

ggplot(Jmax_blups_genotypes, aes(x=adjusted_Jmax)) +
  geom_density()+ 
  xlab("Jmax")+
  ylab("Density")

#Export 

write.csv(Jmax_blups_genotypes,"~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/Jmax_blups.csv", row.names = F)

#Model for Vcmax ----

Vcmax_model <-  lmer(data=LICOR_ACI, Vcmax ~ (1|Name) + (1|Rep) + (1|Rep:Column) + (1|LICOR_user) + 
                      (1|LICOR_time) + (1|LICOR_date) + (1|LICOR_ID))

summary(Vcmax_model)

var_Vcmax <- as.data.frame(VarCorr(Vcmax_model, comp = "vcov"))
head(var_Vcmax)

h_Vcmax <- var_Vcmax[1,4]/sum(var_Vcmax[,4])

#sort out BLUPS

Vcmax_blups <- ranef(Vcmax_model)
Vcmax_blups_genotypes <- Vcmax_blups$Name
Vcmax_blups_genotypes$Name <- rownames(Vcmax_blups_genotypes)
rownames(Vcmax_blups_genotypes) <- NULL
colnames(Vcmax_blups_genotypes) <- c("Vcmax", "Line")
Vcmax_blups_genotypes$adjusted_Vcmax <- Vcmax_blups_genotypes$Vcmax + mean(LICOR_ACI$Vcmax, na.rm = TRUE)

#Density plot 

ggplot(Vcmax_blups_genotypes, aes(x=adjusted_Vcmax)) +
  geom_density()+ 
  xlab("Vcmax")+
  ylab("Density")

#Export 

write.csv(Vcmax_blups_genotypes,"~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/Vcmax_blups.csv", row.names = F)

