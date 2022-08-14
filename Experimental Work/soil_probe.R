#Set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Experimental Work/Drought Experiment")

#Get packages ----

library(readxl)
library(ggplot2)

#Get data ----

soil_data <- read_excel("Soil Probe Data.xlsx")
soil_data$Treatment <- as.factor(soil_data$Treatment)
soil_data$Date <- substr(soil_data$Date, 1,10)

day_1_40 <- subset(soil_data, Date == "2022-04-14" & Treatment == "40")
day_1_80 <- subset(soil_data, Date == "2022-04-14" & Treatment == "80")

day_2_40 <- subset(soil_data, Date == "2022-04-18" & Treatment == "40")
day_2_80 <- subset(soil_data, Date == "2022-04-18" & Treatment == "80")

day_3_40 <- subset(soil_data, Date == "2022-04-23" & Treatment == "40")
day_3_80 <- subset(soil_data, Date == "2022-04-23" & Treatment == "80")

day_4_40 <- subset(soil_data, Date == "2022-05-01" & Treatment == "40")
day_4_80 <- subset(soil_data, Date == "2022-05-01" & Treatment == "80")

day_5_40 <- subset(soil_data, Date == "2022-05-11" & Treatment == "40")
day_5_80 <- subset(soil_data, Date == "2022-05-11" & Treatment == "80")

soil_40 <- subset(soil_data, Treatment == "40")
soil_80 <- subset(soil_data, Treatment == "80")

t.test(soil_40$Average, soil_80$Average, var.equal = T)

t.test(day_1_40$Average, day_1_80$Average, var.equal = T)
t.test(day_2_40$Average, day_2_80$Average, var.equal = T)
t.test(day_3_40$Average, day_3_80$Average, var.equal = T)
t.test(day_4_40$Average, day_4_80$Average, var.equal = T)
t.test(day_5_40$Average, day_5_80$Average, var.equal = T)

#Plot data ----

ggplot(soil_data, aes(x=Treatment, y=Average, color = Treatment)) + geom_boxplot() +
  ylab("% Volume Water Content") +
  xlab("Treatment") +
  ylim(0,40) +
  scale_color_manual(values=c("red","blue")) +
  theme(axis.text.x=element_text(size=12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

ggplot(soil_data, aes(x=Treatment, y=Average, color = Treatment)) + geom_boxplot() +
  facet_wrap(~`Location Type` + Genotype , ncol = 4) + ylab("% Volume Water Content")

soil_data$`Location Type` <- as.factor(soil_data$`Location Type`)

two_way <- aov(Average ~ Treatment + `Location Type`, data = soil_data)
