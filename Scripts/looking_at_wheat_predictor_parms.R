#set working directory ----
setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Phenotyping Campaign")

#get libraries ----
library(readr)
library(readxl)
library(stringr)
library(ggplot2)

#get data ----
parms <- read_csv("wheat_predictor_parms.csv")
field_design <- read_excel("FieldDesign.xlsx")

#split the parms data into plot and repeat columns

parms[c('Plot', 'Repeat')] <- str_split_fixed(parms$ID, ' ',2)
parms <- parms[,c(12,13,2:11)]
blups <- read_csv("BLUPs_for_locations.csv")

#get means for the parms data on a plot by plot basis

parms_means <- aggregate(parms[,3:12], list(parms$Plot), mean)
names(parms_means)[1] <- "PlotID"

#join the field design data and the mean parameter values

joint <- merge(field_design, parms_means, by = "PlotID")
joint <- joint[,c(6,1,9:18)]

joint <- aggregate(joint[,3:12],list(joint$Name), mean)
names(joint)[1] <- "Genotype"

#get blups variables that relate to parameters from wheat predictor site 

blups <- blups[,c(1,3,4,5,20,21)]
names(blups)[1] <- "Genotype"

#joint into dataset (blups and parm means) so that can compare means to blups 

joint_blups <- merge(joint, blups, by = "Genotype")

ggplot(joint_blups, aes(x=LMA, y=SLA_blups)) + geom_point()
ggplot(na.omit(joint_blups), aes(x=Vcmax, y=TPU_Vcmax)) + geom_point()
ggplot(na.omit(joint_blups), aes(x=J, y=TPU_Jmax)) + geom_point()
ggplot(na.omit(joint_blups), aes(x=Cond, y=gs)) + geom_point()
ggplot(na.omit(joint_blups), aes(x=Photo, y=asat)) + geom_point()

