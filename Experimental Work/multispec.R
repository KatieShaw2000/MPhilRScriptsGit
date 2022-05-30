#Set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Experimental Work/Drought Experiment")

#Get packages ----

library(ggplot2)
library(readxl)

#Get data ----

multispec_20th <- read_csv("Multispec/multispec 20Th April.csv")
multispec_20th <- multispec_20th[,c(24,13)]
names(multispec_20th)[1] <- "Pot"
names(multispec_20th)[2] <- "PhiPSII_20th"
 
multispec_30th <- read_csv("Multispec/multispec 30th April.csv")
multispec_30th <- multispec_30th[,c(24,13)]
names(multispec_30th)[1] <- "Pot"
names(multispec_30th)[2] <- "PhiPSII_30th"

info <- read_excel("glasshouse_info.xlsx")
names(info)[1] <- "Pot"

multispec<- merge(info, multispec_20th, by = "Pot")
multispec <- merge(multispec, multispec_30th, by = "Pot")

multispec$Treatment <- as.factor(multispec$Treatment)
names(multispec)[3] <- "Location"

multispec$combined <- rowMeans(multispec[,c(5,6)])

#Plot the data ----

order_location_type <- within(multispec, Genotype <- factor(Genotype, levels=c("B1K-05-12", 
                                                                                  "B1K-05-08",
                                                                                  "B1K-12-10",
                                                                                  "B1K-04-03",
                                                                                  "B1K-03-17",
                                                                                  "B1K-17-17",
                                                                                  "B1K-10-01",
                                                                                  "B1K-49-10")))
ggplot(order_location_type, aes(x=Treatment, y=PhiPSII_20th)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4)

ggplot(order_location_type, aes(x=Treatment, y=PhiPSII_30th)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4)

ggplot(order_location_type, aes(x=Treatment, y=combined, color = Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4) + ylab("PhiPSII")
