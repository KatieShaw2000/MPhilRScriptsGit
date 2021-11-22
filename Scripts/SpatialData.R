#Get packages ----

library(readxl)
library(tidyverse)

#Set working directory ----

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign")

#Load template 

mrbean_data <- read_excel("~/OneDrive - University of Cambridge/MPhil/GitLink/all_parms_raw.xlsx")

#Get SLA dataframe

SLA <- read_excel("SLA Data/SLA.xlsx")
SLA <- filter(SLA, ToDrop == "Keep") #filter out outliers I picked manually 
SLA <- select(SLA, -ToDrop)
dates <- read_excel("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/HeadingSamplingDates.xlsx")
dates <- select(dates, Name, Rep, "Heading-Sampling", "Transplanting-Heading", Sampling)
colnames(dates) <- c("Name", "Rep", "heading_to_sampling","transplanting_to_heading", "sampling_date")
mergeSLA <- merge(SLA, dates, by = c("Name", "Rep"))

mr_bean_SLA <- merge(mrbean_data, mergeSLA, by = c("Name", "Plot", "Rep", "Repeat"))
write.csv(mr_bean_SLA,"~/OneDrive - University of Cambridge/MPhil/Mr Bean/MrBean_SLA.csv")

#Get heading date dataframe

mr_bean_heading_date <- merge(mrbean_data, dates, by = c("Name", "Rep"))
write.csv(mr_bean_heading_date,"~/OneDrive - University of Cambridge/MPhil/Mr Bean/MrBean_heading_date.csv")

#Load ACi data

ACi <- read_excel("~/OneDrive - University of Cambridge/MPhil/GitLink/ACi_raw_parms.xlsx")
ACi <- ACi[1:243,1:10]ß

mr_bean_ACi <- merge(mrbean_data, ACi, by = c("Name", "Plot", "Repeat", "Rep"))
write.csv(mr_bean_ACi,"~/OneDrive - University of Cambridge/MPhil/Mr Bean/MrBean_ACi.csv")
