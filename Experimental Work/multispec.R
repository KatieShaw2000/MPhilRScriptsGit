#Set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Experimental Work/Drought Experiment")

#Get packages ----

library(ggplot2)
library(readxl)
library(AICcmodavg)

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

ggplot(multispec, aes(x=PhiPSII_20th)) + geom_density()
ggplot(multispec, aes(x=PhiPSII_30th)) + geom_density()

#Export data ----

write.csv(multispec, "All Parameters/multispec.csv")

#Plot the data ----

order_location_type <- within(multispec, Genotype <- factor(Genotype, levels=c("B1K-05-12", 
                                                                                  "B1K-05-08",
                                                                                  "B1K-12-10",
                                                                                  "B1K-04-03",
                                                                                  "B1K-03-17",
                                                                                  "B1K-17-17",
                                                                                  "B1K-10-01",
                                                                                  "B1K-49-10")))

order_basic <- within(multispec, Location <- factor(Location, levels = c("Desert", "Coastal")))

phipsii_plot1 <- ggplot(order_location_type, aes(x=Treatment, y=combined, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4)+
  theme(legend.position = "none") +
  ylab("PhiPSII") +
  scale_color_manual(values=c("red","blue")) 

phipsii_plot2 <- ggplot(order_basic, aes(x=Treatment, y=combined, color = Treatment)) + geom_boxplot() +
  facet_wrap(~Location) + 
  ylab("PhiPSII") +
  scale_color_manual(values=c("red","blue"))

ggarrange(phipsii_plot1, phipsii_plot2, ncol = 2, labels = c("A", "B"))

phipsii_interaction <- aov(combined ~ Location*Treatment, data = multispec)

shapiro.test(residuals(lm(combined ~ Location*Treatment, data=multispec)))

ggplot(multispec, aes(x=combined)) + geom_density()

plot(phipsii_interaction)

summary(phipsii_interaction)

mean_phipsii_desert_40 <- mean(multispec[multispec$Location == "Desert" & multispec$Treatment == "40",]$combined)
mean_phipsii_desert_80 <- mean(multispec[multispec$Location == "Desert" & multispec$Treatment == "80",]$combined)
mean_phipsii_coast_40 <- mean(multispec[multispec$Location == "Coastal" & multispec$Treatment == "40",]$combined)
mean_phipsii_coast_80 <- mean(multispec[multispec$Location == "Coastal" & multispec$Treatment == "80",]$combined)







