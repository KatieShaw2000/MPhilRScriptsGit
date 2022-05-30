#Set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Experimental Work/Drought Experiment/6400s_for_R")

#Get packages ----

library(ggplot2)
library(readxl)

#Get data ----

SLA <- read_excel("Leaf masses and areas.xlsx")
names(SLA)[2] <- "Treatment"
names(SLA)[3] <- "Location"
SLA$Treatment <- as.factor(SLA$Treatment)
names(SLA)[5] <-"fresh_mass"
names(SLA)[6] <- "dry_mass"
names(SLA)[7] <- "leaf_area"

SLA$fresh_mass_per_area <- SLA$fresh_mass/SLA$leaf_area

#Plot data ----

order_location_type <- within(SLA, Genotype <- factor(Genotype, levels=c("B1K-05-12", 
                                                                               "B1K-05-08",
                                                                               "B1K-12-10",
                                                                               "B1K-04-03",
                                                                               "B1K-03-17",
                                                                               "B1K-17-17",
                                                                               "B1K-10-01",
                                                                               "B1K-49-10")))
ggplot(order_location_type, aes(x=Treatment, y=fresh_mass, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4) + ylab("Fresh Mass (g)")

ggplot(order_location_type, aes(x=Treatment, y=dry_mass, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4) + ylab("Dry Mass (g)")

ggplot(order_location_type, aes(x=Treatment, y=dry_mass, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4) + ylab("Dry Mass (g)")

ggplot(order_location_type, aes(x=Treatment, y=leaf_area, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4) + ylab(expression(paste("Leaf Area (cm"^"2",")")))

ggplot(order_location_type, aes(x=Treatment, y=SLA, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4)

ggplot(order_location_type, aes(x=Treatment, y=fresh_mass_per_area, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4)
