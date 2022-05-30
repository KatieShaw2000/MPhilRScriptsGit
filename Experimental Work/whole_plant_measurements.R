#Set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Experimental Work/Drought Experiment")

#Get packages ----

library(ggplot2)
library(readxl)

#Get data ----

area <- read_excel("Total leaf area.xlsx")
area <- area[,c(1,6)]
mass <- read_excel("Total leaf mass.xlsx")

combined <- merge(mass, area, by = "Number")
names(combined)[3] <- "Location"
combined$Treatment <- as.factor(combined$Treatment)

#Plot them ----

order_location_type <- within(combined, Genotype <- factor(Genotype, levels=c("B1K-05-12", 
                                                                         "B1K-05-08",
                                                                         "B1K-12-10",
                                                                         "B1K-04-03",
                                                                         "B1K-03-17",
                                                                         "B1K-17-17",
                                                                         "B1K-10-01",
                                                                         "B1K-49-10")))

ggplot(order_location_type, aes(x=Treatment, y=`Dried mass`, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4) + ylab("Total aboveground dried biomass (g)")

ggplot(order_location_type, aes(x=Treatment, y=`Total leaf area`, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4) + ylab(expression(paste("Total Leaf Area (cm"^"2",")")))
