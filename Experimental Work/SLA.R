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
names(SLA)[9] <- "fresh_dry"


SLA$SLA <- SLA$SLA/100
SLA$SLA <- SLA$SLA/100


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
  ylim(0,0.042) +
  ylab(expression(paste("SLA (m"^"2","g"^"-1",")")))+
  scale_color_manual(values=c("red","blue")) 

SLA_plot2 <- ggplot(order_basic, aes(x=Treatment, y=SLA, color = Treatment)) + geom_boxplot() +
  ylim(0,0.042) +
  facet_wrap(~Location) + ylab(expression(paste("SLA (m"^"2","g"^"-1",")"))) +
  scale_color_manual(values=c("red","blue")) 

ggarrange(SLA_plot1, SLA_plot2, ncol = 2, labels = c("A", "B"))

#some stats tests----

SLA_interaction <- aov(SLA ~ Location * Treatment, data = SLA)

shapiro.test(residuals(lm(SLA ~ Location * Treatment, data = SLA))) #normally distributed

plot(SLA_interaction) #look at diagnostic plots- they look okay

summary(SLA_interaction) #report this result 

SLA_nested <- aov(SLA ~ Location/Genotype + Location * Treatment, data = SLA)

mean_SLA_desert_40 <- mean(SLA[SLA$Location == "Desert" & SLA$Treatment == "40",]$SLA)
mean_SLA_desert_80 <- mean(SLA[SLA$Location == "Desert" & SLA$Treatment == "80",]$SLA)
mean_SLA_coast_40 <- mean(SLA[SLA$Location == "Coastal" & SLA$Treatment == "40",]$SLA)
mean_SLA_coast_80<- mean(SLA[SLA$Location == "Coastal" & SLA$Treatment == "80",]$SLA)

#fresh mass:dry mass measurements for thesis ----

ratio_plot1 <- ggplot(order_location_type, aes(x=Treatment, y=fresh_dry, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4)+
  theme(legend.position = "none") +
  ylab("Fresh mass : Dry mass") +
  scale_color_manual(values=c("red","blue")) 

ratio_plot2 <- ggplot(order_basic, aes(x=Treatment, y=fresh_dry, color = Treatment)) + geom_boxplot() +
  facet_wrap(~Location) + ylab(expression(paste("SLA (m"^"2","g"^"-1",")"))) +
  ylab("Fresh mass : Dry mass") +
  scale_color_manual(values=c("red","blue")) 

ggarrange(ratio_plot1, ratio_plot2, ncol = 2, labels = c("A", "B"))

ratio_interaction <- aov(fresh_dry ~ Location * Treatment, data = SLA)

shapiro.test(residuals(lm(fresh_dry ~ Location * Treatment, data = SLA))) #not normally distributed 

plot(ratio_interaction)

SLA <- SLA[-c(95,32,61),]

ratio_interaction <- aov(fresh_dry ~ Location * Treatment, data = SLA)

shapiro.test(residuals(lm(fresh_dry ~ Location * Treatment, data = SLA))) #now normally distirbued 

summary(ratio_interaction)


mean_ratio_desert_40 <- mean(SLA[SLA$Location == "Desert" & SLA$Treatment == "40",]$fresh_dry)
mean_ratio_desert_80 <- mean(SLA[SLA$Location == "Desert" & SLA$Treatment == "80",]$fresh_dry)
mean_ratio_coast_40 <- mean(SLA[SLA$Location == "Coastal" & SLA$Treatment == "40",]$fresh_dry)
mean_ratio_coast_80<- mean(SLA[SLA$Location == "Coastal" & SLA$Treatment == "80",]$fresh_dry)

