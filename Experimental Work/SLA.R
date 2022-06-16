#Set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Experimental Work/Drought Experiment")

#Get packages ----

library(ggplot2)
library(readxl)
library(AICcmodavg)
library(ggpubr)

#Get data ----

SLA <- read_excel("Leaf masses and areas.xlsx")
names(SLA)[2] <- "Treatment"
names(SLA)[3] <- "Location"
SLA$Treatment <- as.factor(SLA$Treatment)
names(SLA)[5] <-"fresh_mass"
names(SLA)[6] <- "dry_mass"
names(SLA)[7] <- "leaf_area"

SLA$fresh_mass_per_area <- SLA$fresh_mass/SLA$leaf_area

#Export SLA data ----

write.csv(SLA,"All Parameters/SLA.csv")

#Plot data ----

order_location_type <- within(SLA, Genotype <- factor(Genotype, levels=c("B1K-05-12", 
                                                                               "B1K-05-08",
                                                                               "B1K-12-10",
                                                                               "B1K-04-03",
                                                                               "B1K-03-17",
                                                                               "B1K-17-17",
                                                                               "B1K-10-01",
                                                                               "B1K-49-10")))

ggplot(order_location_type, aes(x=Treatment, y=dry_mass, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4) + ylab("Dry Mass (g)") +
  scale_color_manual(values=c("red","blue")) 

ggplot(order_location_type, aes(x=Treatment, y=leaf_area, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4) + ylab(expression(paste("Leaf Area (cm"^"2",")")))+
  scale_color_manual(values=c("red","blue")) 

order_basic <- within(SLA, Location <- factor(Location, levels = c("Desert", "Coastal")))

ggplot(order_basic, aes(x=Treatment, y=dry_mass, color = Treatment)) + geom_boxplot() +
  facet_wrap(~Location) + ylab("Dry Mass (g)")+
  scale_color_manual(values=c("red","blue")) 

ggplot(order_basic, aes(x=Treatment, y=leaf_area, color = Treatment)) + geom_boxplot() +
  facet_wrap(~Location) + ylab(expression(paste("Leaf Area (cm"^"2",")"))) +
  scale_color_manual(values=c("red","blue")) 

#plots to use in thesis ----

SLA_plot1 <- ggplot(order_location_type, aes(x=Treatment, y=SLA, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4)+
  theme(legend.position = "none") +
  ylab(expression(paste("SLA (cm"^"2","g"^"-1",")")))+
  scale_color_manual(values=c("red","blue")) 

SLA_plot2 <- ggplot(order_basic, aes(x=Treatment, y=SLA, color = Treatment)) + geom_boxplot() +
  facet_wrap(~Location) + ylab(expression(paste("SLA (cm"^"2","g"^"-1",")"))) +
  scale_color_manual(values=c("red","blue")) 

ggarrange(SLA_plot1, SLA_plot2, ncol = 2, labels = c("A", "B"))

#some stats tests----

SLA_two_way <- aov(SLA ~ Location + Treatment, data = SLA)
SLA_interaction <- aov(SLA ~ Location * Treatment, data = SLA)
SLA_nested <- aov(SLA ~ Location/Genotype + Treatment, data = SLA)
SLA_nested_interaction <- aov(SLA ~ Location/Genotype + Treatment + Location*Treatment, data=SLA)

SLA_models <- list(SLA_two_way, SLA_interaction, SLA_nested, SLA_nested_interaction)
SLA_models_names <- c("two_way", "interaction", "nested", "both")

aictab(SLA_models, modnames = SLA_models_names)

plot(SLA_nested) #look at diagnostic plots- they look okay

mean_SLA_desert_40 <- mean(SLA[SLA$Location == "Desert" & SLA$Treatment == "40",]$SLA)
mean_SLA_desert_80 <- mean(SLA[SLA$Location == "Desert" & SLA$Treatment == "80",]$SLA)
mean_SLA_coast_40 <- mean(SLA[SLA$Location == "Coastal" & SLA$Treatment == "40",]$SLA)
mean_SLA_coast_80<- mean(SLA[SLA$Location == "Coastal" & SLA$Treatment == "80",]$SLA)
