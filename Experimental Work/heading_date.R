#Set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Experimental Work/Drought Experiment")

#Get packages ----

library(ggplot2)
library(readxl)
library(dplyr)

#Get data ----

heading_date <- read_excel("Heading Date.xlsx")

#Export data ----

write.csv(heading_date, "All Parameters/heading_date.csv")

#Back to data ----

heading_date <- heading_date[-c(71,97,98,103,105,106,107,109),]

heading_date <- heading_date[,-c(5,6)]
heading_date$`Days to heading` <- as.numeric(heading_date$`Days to heading`)

summary(heading_date)

#Get some numbers and plots ----

heading_date <- na.omit(heading_date)

heading_date$Treatment <- as.factor(heading_date$Treatment)
heading_date$`Location Type` <- as.factor(heading_date$`Location Type`)
heading_date$Genotype <- as.factor(heading_date$Genotype)

names(heading_date)[3] <- "Location"

summary(heading_date)

order_location_type <- within(heading_date, Genotype <- factor(Genotype, levels=c("B1K-05-12", 
                                                                         "B1K-05-08",
                                                                         "B1K-04-03",
                                                                         "B1K-10-01")))

give.n <- function(x){
  return(c(y=median(x) +1, label=length(x))) #y shows the height I want each text
}

ggplot(order_location_type, aes(x=Treatment, y=`Days to heading`, color=Treatment)) + geom_boxplot() +
  scale_color_manual(values=c("red","blue")) +
  stat_summary(fun.data = give.n, geom="text", color = "black", size=3)+
  facet_wrap(~Genotype + Location, ncol=4) + ylab("Days to Heading")
