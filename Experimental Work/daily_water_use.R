#Set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Experimental Work/Drought Experiment")

#Get packages ----

library(ggplot2)
library(readxl)
library(dplyr)
library(reshape2)
library(AICcmodavg)
library(ggpubr)

#Get data ----

control_pots <- read_excel("Daily Soil Water Data Control Pots.xlsx")
exp_pots <- read_excel("Daily Soil Water Data Experimental Pots.xlsx")

#Get daily mean for control pots for the different treatments ---- 

control_40 <- subset(control_pots, Treatment == "40")
control_80 <- subset(control_pots, Treatment == "80")

mean_control_40 <- colMeans(control_40[,3:40], na.rm = TRUE)
mean_control_80 <- colMeans(control_80[,3:40], na.rm = TRUE)

#Get new dataframe that has the daily soil water data, but with control means taken off ----

exp_40 <- subset(exp_pots, Treatment == "40") #have to remove the names columns so they're the same size
exp_40 <- exp_40[,c(5:42)]

exp_80 <- subset(exp_pots, Treatment == "80")
exp_80 <- exp_80[,c(5:42)]

mean_control_40 <- as.data.frame(mean_control_40)
mean_control_40 <- t(mean_control_40)
mean_control_40 <- as.data.frame(mean_control_40)
mean_control_40 <- as.data.frame(lapply(mean_control_40, rep, 60))

new_exp_40 <- exp_40 - mean_control_40

mean_control_80 <- as.data.frame(mean_control_80)
mean_control_80 <- t(mean_control_80)
mean_control_80 <- as.data.frame(mean_control_80)
mean_control_80 <- as.data.frame(lapply(mean_control_80, rep, 60))

new_exp_80 <- exp_80 - mean_control_80

test <- subset(exp_pots, Treatment == "40") #need to add back in the names!
test <- test[,1:4]

final_exp_40 <- c(test, new_exp_40)
final_exp_40 <- as.data.frame(final_exp_40)

test2 <- subset(exp_pots, Treatment == "80")                              
test2 <- test2[,1:4]

final_exp_80 <- c(test2, new_exp_80)
final_exp_80 <- as.data.frame(final_exp_80)

final_water_data <- rbind(final_exp_40, final_exp_80)

rm(control_40, control_80, control_pots, exp_40, exp_80, exp_pots, mean_control_40, mean_control_80) #tidy up
rm(test, test2, new_exp_40, new_exp_80)

colnames(final_water_data) <- c("Number", "Treatment", "Location", "Genotype", "1", "2", "3", "4", "5", "6",
                                "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                "21", "22", "23","24", "25","26","27","28","29","30","31","32","33","34","35", "36", "37","38")

final_water_data <- final_water_data[,-c(5,33)]#remove day 29 as this data is really weird!! also 1 as can't interpolate negative values here!
final_water_data <- final_water_data[order(final_water_data$Number),]
row.names(final_water_data) <- final_water_data$Number

#interpolate any negative values ----

summary(final_water_data) #see which columns have negative values in for min values to interpolate!

final_water_data[45,7] <- "2.13"
final_water_data[69,7] <- "12.92"
final_water_data[final_water_data$Number == "111",7] <- "2.77"
final_water_data[final_water_data$Number == "72",8] <- "2.67"
final_water_data[final_water_data$Number == "25", 9] <- "7.72"
final_water_data[final_water_data$Number == "22", 11] <- "10.63"
final_water_data[final_water_data$Number == "45", 11] <- "0.65"
final_water_data[final_water_data$Number == "55", 12] <- "3.50"
final_water_data[final_water_data$Number == "101", 15] <- "1.41"

final_water_data <- final_water_data[final_water_data$Number != "85",] #drop pot 85 as had loads of negatives- couldn't interpolate all!

summary(final_water_data)

final_water_data <- final_water_data[final_water_data$Number != "72",] #also remove pot 72 for the same reason

summary(final_water_data)

final_water_data[final_water_data$Number == "121", 23] <- "37.84"
final_water_data[final_water_data$Number == "35", 32] <- "7.14"

final_water_data[5:40] <- lapply(final_water_data[5:40], as.numeric)

summary(final_water_data)

final_water_data$Treatment <- as.factor(final_water_data$Treatment)
final_water_data$Genotype <- as.factor(final_water_data$Genotype)

#make final water data into litres not ml ----

final_water_data[,5:40] <- final_water_data[,5:40]/1000

#plot daily water use ----

transpose <- as.data.frame(t(final_water_data))
transpose <- transpose[-c(1:4),]
transpose$day <- rownames(transpose)
transpose <- melt(transpose, id.vars = c("day"))
transpose$value <- as.numeric(transpose$value)
names(transpose)[1] <- "Day" 
names(transpose)[2] <- "Number"
names(transpose)[3] <- "daily_water_use"

info <- final_water_data[,1:4]

merged <- merge(transpose, info, by = "Number")
merged$Treatment <- as.factor(merged$Treatment)
merged$Day <- as.numeric(merged$Day)
merged$Location <- as.factor(merged$Location)

#need to add in additional information into transpose dataframe for good plotting!

order_merged <- within(merged, Genotype <- factor(Genotype, levels=c("B1K-05-12", 
                                                                               "B1K-05-08",
                                                                               "B1K-12-10",
                                                                               "B1K-04-03",
                                                                               "B1K-03-17",
                                                                               "B1K-17-17",
                                                                               "B1K-10-01",
                                                                               "B1K-49-10")))

ggplot(order_merged, aes(x=Day, y=daily_water_use, color=Treatment)) +
  xlab("Day of Watering Treatment") +
  ylab("Daily Water Use (l)") + geom_point() + geom_line(aes(x=Day, y=daily_water_use, group=Number)) +
  facet_wrap(~Genotype + Location, ncol=4)+
  scale_color_manual(values=c("red","blue")) 

#based on this graph I don't think average daily water use is appropriate? Total water use sure

#get total lifetime water use based on days I measured and do some plots ----

final_water_data$total_water_use <- rowSums(final_water_data[,5:40])

order_data <- within(final_water_data, Genotype <- factor(Genotype, levels=c("B1K-05-12", 
                                                                     "B1K-05-08",
                                                                     "B1K-12-10",
                                                                     "B1K-04-03",
                                                                     "B1K-03-17",
                                                                     "B1K-17-17",
                                                                     "B1K-10-01",
                                                                     "B1K-49-10")))

order_basic <- within(final_water_data, Location <- factor(Location, levels = c("Desert", "Coastal")))

total_plot1 <- ggplot(order_data, aes(x=Treatment, y=total_water_use, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4)+
  theme(legend.position = "none") +
  ylab("Total Water Use (l)") +
  scale_color_manual(values=c("red","blue")) 

total_plot2 <- ggplot(order_basic, aes(x=Treatment, y=total_water_use, color = Treatment)) + geom_boxplot() +
  facet_wrap(~Location) + 
  ylab("Total Water Use (l)") +
  scale_color_manual(values=c("red","blue"))

ggarrange(total_plot1, total_plot2, ncol = 2, labels = c("A", "B"))

mean_total_desert_40 <- mean(final_water_data[final_water_data$Location == "Desert" & final_water_data$Treatment == "40",]$total_water_use)
mean_total_desert_80 <- mean(final_water_data[final_water_data$Location == "Desert" & final_water_data$Treatment == "80",]$total_water_use)
mean_total_coast_40 <- mean(final_water_data[final_water_data$Location == "Coastal" & final_water_data$Treatment == "40",]$total_water_use)
mean_total_coast_80 <- mean(final_water_data[final_water_data$Location == "Coastal" & final_water_data$Treatment == "80",]$total_water_use)


total_interaction <- aov(total_water_use ~ Location * Treatment, data = final_water_data)

shapiro.test(residuals(lm(log10(total_water_use)~Location*Treatment, data=final_water_data)))

plot(total_interaction)

summary(total_interaction)

#get the biomass data for normalising and average final 3 days of data ----

final_water_data$final_average <- rowMeans(final_water_data[,38:40])

mass <- read_excel("Total leaf mass.xlsx")
mass <- mass[,c(1,5)]

mass_water <- merge(final_water_data, mass, by = "Number")

mass_water$normalised <- mass_water$final_average/mass_water$`Dried mass`

mass_water$WUE <- mass_water$`Dried mass`/mass_water$final_average

mass_water$life_WUE <- mass_water$`Dried mass`/mass_water$total_water_use

#do some plots for normalised water use ----

order_mass_water <- within(mass_water, Genotype <- factor(Genotype, levels=c("B1K-05-12", 
                                                                             "B1K-05-08",
                                                                             "B1K-12-10",
                                                                             "B1K-04-03",
                                                                             "B1K-03-17",
                                                                             "B1K-17-17",
                                                                             "B1K-10-01",
                                                                             "B1K-49-10")))

order_basic_mass_water <- within(mass_water, Location <- factor(Location, levels = c("Desert", "Coastal")))

norm_plot1 <- ggplot(order_mass_water, aes(x=Treatment, y=normalised, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4)+
  theme(legend.position = "none") +
  ylab(expression(paste("Water use per unit aboveground dry mass (l"," g"^"-1","day"^"-1",")"))) +
  scale_color_manual(values=c("red","blue")) 

norm_plot2 <- ggplot(order_basic_mass_water, aes(x=Treatment, y=normalised, color = Treatment)) + geom_boxplot() +
  facet_wrap(~Location) + 
  ylab(expression(paste("Water use per unit aboveground dry mass (l"," g"^"-1","day"^"-1",")"))) +
  scale_color_manual(values=c("red","blue"))

ggarrange(norm_plot1, norm_plot2, ncol = 2, labels = c("A", "B"))

WUE_plot1 <- ggplot(order_mass_water, aes(x=Treatment, y=WUE, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4)+
  theme(legend.position = "none") +
  ylab(expression(paste("WUE (g"," ml"^"-1",")"))) +
  scale_color_manual(values=c("red","blue")) 

WUE_plot2 <- ggplot(order_basic_mass_water, aes(x=Treatment, y=WUE, color = Treatment)) + geom_boxplot() +
  facet_wrap(~Location) + 
  ylab(expression(paste("WUE (g"," ml"^"-1",")"))) +
  scale_color_manual(values=c("red","blue"))

ggarrange(WUE_plot1, WUE_plot2, ncol = 2, labels = c("A", "B"))

WUE_interaction <- aov(WUE ~ Location * Treatment, data = mass_water)

shapiro.test(residuals(lm(WUE~Location*Treatment, data=mass_water)))

summary(WUE_interaction)

mean_normalised_desert_40 <- mean(mass_water[mass_water$Location == "Desert" & mass_water$Treatment == "40",]$normalised)
mean_normalised_desert_80 <- mean(mass_water[mass_water$Location == "Desert" & mass_water$Treatment == "80",]$normalised)
mean_normalised_coast_40 <- mean(mass_water[mass_water$Location == "Coastal" & mass_water$Treatment == "40",]$normalised)
mean_normalised_coast_80 <- mean(mass_water[mass_water$Location == "Coastal" & mass_water$Treatment == "80",]$normalised)


norm_interaction <- aov(normalised ~ Location + Treatment + Location * Treatment, data = mass_water)

shapiro.test(residuals(lm(normalised~Location*Treatment, data=mass_water)))

plot(norm_interaction)

summary(norm_interaction)

#export data ----

write.csv(mass_water[,c(1:4,41,44)], "All Parameters/water_use.csv")
