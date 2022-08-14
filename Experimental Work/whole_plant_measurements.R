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

combined$`Total leaf area` <- combined$`Total leaf area`/100
combined$`Total leaf area` <- combined$`Total leaf area`/100

#Export data ----

write.csv(combined, "All Parameters/whole_plant.csv")

#Plot them ----

order_location_type <- within(combined, Genotype <- factor(Genotype, levels=c("B1K-05-12", 
                                                                         "B1K-05-08",
                                                                         "B1K-12-10",
                                                                         "B1K-04-03",
                                                                         "B1K-03-17",
                                                                         "B1K-17-17",
                                                                         "B1K-10-01",
                                                                         "B1K-49-10")))

order_basic <- within(combined, Location <- factor(Location, levels = c("Desert", "Coastal")))

#Get plots for thesis ----

mass_plot1 <- ggplot(order_location_type, aes(x=Treatment, y=`Dried mass`, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4)+
  theme(legend.position = "none") +
  ylab("Total Aboveground Dry Mass (g)") +
  scale_color_manual(values=c("red","blue")) 

mass_plot2 <- ggplot(order_basic, aes(x=Treatment, y=`Dried mass`, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Location, ncol=4)+
  ylab("Total Aboveground Dry Mass (g)") +
  scale_color_manual(values=c("red","blue")) 

ggarrange(mass_plot1, mass_plot2, ncol = 2, labels = c("A", "B"))

area_plot1 <- ggplot(order_location_type, aes(x=Treatment, y=`Total leaf area`, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4)+
  theme(legend.position = "none") +
  ylim(0,0.022) +
  ylab(expression(paste("Total Leaf Area (m"^"2",")"))) +
  scale_color_manual(values=c("red","blue")) 

area_plot2 <- ggplot(order_basic, aes(x=Treatment, y=`Total leaf area`, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Location, ncol=4)+
  ylim(0,0.022) +
  ylab(expression(paste("Total Leaf Area (m"^"2",")"))) +
  scale_color_manual(values=c("red","blue")) 

ggarrange(area_plot1, area_plot2, ncol = 2, labels = c("A", "B"))

#Do stats tests for total leaf area ---

area_interaction <- aov(`Total leaf area` ~ Location*Treatment, data = combined)

shapiro.test(residuals(lm(`Total leaf area` ~ Location*Treatment, data = combined)))

plot(area_interaction)

summary(area_interaction)

mean_area_desert_40 <- mean(combined[combined$Location == "Desert" & combined$Treatment == "40",]$`Total leaf area`)
mean_area_desert_80 <- mean(combined[combined$Location == "Desert" & combined$Treatment == "80",]$`Total leaf area`)
mean_area_coast_40 <- mean(combined[combined$Location == "Coastal" & combined$Treatment == "40",]$`Total leaf area`)
mean_area_coast_80 <- mean(combined[combined$Location == "Coastal" & combined$Treatment == "80",]$`Total leaf area`)

#Do stats tests for dried mass ----

mass_interaction <- aov(`Dried mass` ~ Location * Treatment, data = combined)

shapiro.test(residuals(lm(`Dried mass` ~ Location * Treatment, data = combined)))

plot(mass_interaction)

#remove points 94, 105 and 26 as these are highlighted on the normal Q-Q plot

combined <- combined[-c(26,94,105),]

mass_interaction <- aov(`Dried mass` ~ Location * Treatment, data = combined)

shapiro.test(residuals(lm(`Dried mass` ~ Location * Treatment, data = combined)))

plot(mass_interaction)

#remove 19, 25 and 29

combined <- combined[-c(19, 25, 29),]

mass_interaction <- aov(`Dried mass` ~ Location * Treatment, data = combined)

shapiro.test(residuals(lm(`Dried mass` ~ Location * Treatment, data = combined)))

plot(mass_interaction)

summary(mass_interaction)

mean_mass_desert_40 <- mean(combined[combined$Location == "Desert" & combined$Treatment == "40",]$`Dried mass`)
mean_mass_desert_80 <- mean(combined[combined$Location == "Desert" & combined$Treatment == "80",]$`Dried mass`)
mean_mass_coast_40 <- mean(combined[combined$Location == "Coastal" & combined$Treatment == "40",]$`Dried mass`)
mean_mass_coast_80 <- mean(combined[combined$Location == "Coastal" & combined$Treatment == "80",]$`Dried mass`)


