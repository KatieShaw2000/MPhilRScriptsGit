#Set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Experimental Work/Drought Experiment")

#Get packages ----

library(readxl)
library(ggplot2)
library(data.table)
library(tidyverse)
library(writexl)
library(plantecophys)
library(corrplot)
library(AICcmodavg)
library(tidyr)
library(ggpubr)

#get data ----

aci_data <- read_excel("cleaned_6400_data.xlsx")
names(aci_data)[3] <- "Location"
reduced_df <- select(aci_data, Ci, Photo, Cond, Tleaf, PARi, Pot, Genotype, Treatment, Location)

reduced_df$Tleaf <- as.numeric(reduced_df$Tleaf)
reduced_df$PARi <- as.numeric(reduced_df$PARi)

names(reduced_df)[3] <- "gs"
names(reduced_df)[5] <- "PPFD"

#fit the curves ----

write_csv(reduced_df, "~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Experimental Work/Drought Experiment/reduced_df.csv")
to_fit <- read.csv("reduced_df.csv") #doesn't work otherwise for some reason.. 

fitting <- fitacis(to_fit, "Pot", fitmethod = "bilinear")

# #plot the fitted curves ----
# 
# PDFpath1 <- "~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Experimental Work/Drought Experiment/aci_fitting.pdf"
# pdf(file=PDFpath1)
# plot(fitting) 
# dev.off()

#get Vcmax and Jmax ----

fitting_output <- coef(fitting)
photosynth <- fitting_output[,1:3]

#get asat, gs and iWUE ----

saturated <- to_fit[!duplicated(to_fit$Pot),]
saturated <- subset(saturated, select = c("Pot", "Treatment", "Location", "Genotype", "Photo", "gs"))
saturated$iWUE <- saturated$Photo/saturated$gs
names(saturated)[5] <- "asat"

parms <- saturated

#get a400 ----

output <- vector()

for (i in 1:length(fitting)) {
  x <- fitting[[i]]
  x <- x$Photosyn(Ci=400)$ALEAF
  output <- append(output, x)
}

parms$a400 <- output

#get sl ----

parms$sl <- (parms$a400 - parms$asat)/parms$a400

#make a dataframe with all of the parameters in ----

parms <- merge(parms, photosynth, by = "Pot")
parms$Treatment <- as.factor(parms$Treatment)

#replace negative sl with NAs ----

parms$sl <- replace(parms$sl, which(parms$sl <0), NA)

#export data ----

write.csv(parms, "All Parameters/aci_parms.csv")

#order data for plots ----

order_parms <- within(parms, Genotype <- factor(Genotype, levels=c("B1K-05-12", "B1K-05-08",
                                                                   "B1K-12-10",
                                                                   "B1K-04-03",
                                                                   "B1K-03-17",
                                                                   "B1K-17-17",
                                                                   "B1K-10-01",
                                                                   "B1K-49-10")))

order_basic <- within(parms, Location <- factor(Location, levels = c("Desert", "Coastal")))

#asat plots and stats tests----

asat_plot1 <- ggplot(order_parms, aes(x=Treatment, y=asat, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4)+
  theme(legend.position = "none") +
  ylab(expression(paste("asat (",mu, "mol",~ m^2, s^-1,")"))) +
  scale_color_manual(values=c("red","blue")) 

asat_plot2 <- ggplot(order_basic, aes(x=Treatment, y=asat, color = Treatment)) + geom_boxplot() +
  facet_wrap(~Location) + 
  ylab(expression(paste("asat (",mu, "mol",~ m^2, s^-1,")"))) +
  scale_color_manual(values=c("red","blue"))

ggarrange(asat_plot1, asat_plot2, ncol = 2, labels = c("A", "B"))

asat_two_way <- aov(asat ~ Location + Treatment, data = parms)
asat_interaction <- aov(asat ~ Treatment*Location, data = parms)
asat_nested <- aov(asat ~ Location/Genotype + Treatment, data = parms)

asat_models <- list(asat_two_way, asat_interaction, asat_nested)
asat_models_names <- c("two_way","interaction", "nested")

aictab(asat_models, modnames = asat_models_names)

summary(asat_nested)

plot(asat_nested) #look at diagnostic plots- they look okay

mean_asat_desert_40 <- mean(parms[parms$Location == "Desert" & parms$Treatment == "40",]$asat)
mean_asat_desert_80 <- mean(parms[parms$Location == "Desert" & parms$Treatment == "80",]$asat)
mean_asat_coast_40 <- mean(parms[parms$Location == "Coastal" & parms$Treatment == "40",]$asat)
mean_asat_coast_80 <- mean(parms[parms$Location == "Coastal" & parms$Treatment == "80",]$asat)

#vcmax plots and stats tests ----

vcmax_plot1 <- ggplot(order_parms, aes(x=Treatment, y=Vcmax, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4)+
  theme(legend.position = "none") +
  ylab(expression(paste("Vcmax (",mu, "mol",~ m^2, s^-1,")"))) +
  scale_color_manual(values=c("red","blue")) 

vcmax_plot2 <- ggplot(order_basic, aes(x=Treatment, y=Vcmax, color = Treatment)) + geom_boxplot() +
  facet_wrap(~Location) + 
  ylab(expression(paste("Vcmax (",mu, "mol",~ m^2, s^-1,")"))) +
  scale_color_manual(values=c("red","blue"))

ggarrange(vcmax_plot1, vcmax_plot2, ncol = 2, labels = c("A", "B"))

vcmax_two_way <- aov(Vcmax ~ Location + Treatment, data = parms)
vcmax_nested <- aov(Vcmax ~ Location/Genotype + Treatment, data = parms)
vcmax_interaction <- aov(Vcmax ~ Location*Treatment, data = parms)

vcmax_models <- list(vcmax_two_way, vcmax_nested)
vcmax_models_names <- c("two_way", "nested")

aictab(vcmax_models, modnames = vcmax_models_names)

plot(vcmax_two_way)

mean_vcmax_desert_40 <- mean(parms[parms$Location == "Desert" & parms$Treatment == "40",]$Vcmax)
mean_vcmax_desert_80 <- mean(parms[parms$Location == "Desert" & parms$Treatment == "80",]$Vcmax)
mean_vcmax_coast_40 <- mean(parms[parms$Location == "Coastal" & parms$Treatment == "40",]$Vcmax)
mean_vcmax_coast_80 <- mean(parms[parms$Location == "Coastal" & parms$Treatment == "80",]$Vcmax)

range_vcmax_desert_40 <- range(parms[parms$Location == "Desert" & parms$Treatment == "40",]$Vcmax)
range_vcmax_coast_40 <- range(parms[parms$Location == "Coastal" & parms$Treatment == "40",]$Vcmax)

#jmax plots and stats tests ----

jmax_plot1 <- ggplot(order_parms, aes(x=Treatment, y=Jmax, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4)+
  theme(legend.position = "none") +
  ylab(expression(paste("Jmax (",mu, "mol",~ m^2, s^-1,")"))) +
  scale_color_manual(values=c("red","blue")) 

jmax_plot2 <- ggplot(order_basic, aes(x=Treatment, y=Jmax, color = Treatment)) + geom_boxplot() +
  facet_wrap(~Location) + 
  ylab(expression(paste("Jmax (",mu, "mol",~ m^2, s^-1,")"))) +
  scale_color_manual(values=c("red","blue"))

ggarrange(jmax_plot1, jmax_plot2, ncol = 2, labels = c("A", "B"))

jmax_two_way <- aov(Jmax ~ Location + Treatment, data = parms)
jmax_nested <- aov(Jmax ~ Location/Genotype + Treatment, data = parms)
jmax_interaction <- aov(Jmax ~ Location*Treatment, data = parms)

jmax_models <- list(jmax_two_way, jmax_nested)
jmax_models_names <- c("two_way", "nested")

aictab(jmax_models, modnames = jmax_models_names)

plot(jmax_two_way)

mean_jmax_desert_40 <- mean(parms[parms$Location == "Desert" & parms$Treatment == "40",]$Jmax)
mean_jmax_desert_80 <- mean(parms[parms$Location == "Desert" & parms$Treatment == "80",]$Jmax)
mean_jmax_coast_40 <- mean(parms[parms$Location == "Coastal" & parms$Treatment == "40",]$Jmax)
mean_jmax_coast_80 <- mean(parms[parms$Location == "Coastal" & parms$Treatment == "80",]$Jmax)

#let's see which plants had headed and try some stats tests ----

parms$headed <- rep("No")
parms[c(14,17,19,25,26,29,30,23,40,61),12] <- "Yes"

parms_not_headed <- parms[parms$headed == "No",]

parms <- merge(parms, photosynth, by = "Pot")
parms$Treatment <- as.factor(parms$Treatment)

order_parms2 <- within(parms_not_headed, Genotype <- factor(Genotype, levels=c("B1K-05-12", "B1K-05-08",
                                                                   "B1K-12-10",
                                                                   "B1K-04-03",
                                                                   "B1K-03-17",
                                                                   "B1K-17-17",
                                                                   "B1K-10-01",
                                                                   "B1K-49-10")))

order_basic2 <- within(parms_not_headed, Location <- factor(Location, levels = c("Desert", "Coastal")))

#trying asat without headed plants ---- 

asat_two_way2 <- aov(asat ~ Location + Treatment, data = parms_not_headed)
asat_nested2 <- aov(asat ~ Location/Genotype + Treatment, data = parms_not_headed)
asat_interaction2 <- aov(asat ~ Location*Treatment, data = parms_not_headed)

#trying vcmax without headed plants ---- 

vcmax_two_way2 <- aov(Vcmax ~ Location + Treatment, data = parms_not_headed)
vcmax_interaction2 <- aov(Vcmax ~ Location + Treatment + Location*Treatment, data = parms_not_headed)
vcmax_nested2 <- aov(Vcmax ~ Location/Genotype + Treatment, data = parms_not_headed)

vcmax_models2 <- list(vcmax_two_way2, vcmax_interaction2, vcmax_nested2)
vcmax_models_names2 <- c("two_way", "interaction", "nested")

aictab(vcmax_models2, modnames = vcmax_models_names2)

summary(vcmax_two_way2)

vcmax_plot3 <- ggplot(order_parms2, aes(x=Treatment, y=Vcmax, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4)+
  theme(legend.position = "none") +
  ylab(expression(paste("Vcmax, no headed plants (",mu, "mol",~ m^2, s^-1,")"))) +
  scale_color_manual(values=c("red","blue")) 

vcmax_plot4 <- ggplot(order_basic2, aes(x=Treatment, y=Vcmax, color = Treatment)) + geom_boxplot() +
  facet_wrap(~Location) + 
  ylab(expression(paste("Vcmax, no headed plants (",mu, "mol",~ m^2, s^-1,")"))) +
  scale_color_manual(values=c("red","blue"))

ggarrange(vcmax_plot3, vcmax_plot4, ncol = 2, labels = c("A", "B"))

#trying jmax without headed plants ---- 

jmax_two_way2 <- aov(Jmax ~ Location + Treatment, data = parms_not_headed)
jmax_interaction2 <- aov(Jmax ~ Location + Treatment + Location*Treatment, data = parms_not_headed)
jmax_nested2 <- aov(Jmax ~ Location/Genotype + Treatment, data = parms_not_headed)

summary(jmax_two_way2)

jmax_plot3 <- ggplot(order_parms2, aes(x=Treatment, y=Jmax, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4)+
  theme(legend.position = "none") +
  ylab(expression(paste("Jmax, no headed plants (",mu, "mol",~ m^2, s^-1,")"))) +
  scale_color_manual(values=c("red","blue")) 

jmax_plot4 <- ggplot(order_basic2, aes(x=Treatment, y=Jmax, color = Treatment)) + geom_boxplot() +
  facet_wrap(~Location) + 
  ylab(expression(paste("Jmax, no headed plants (",mu, "mol",~ m^2, s^-1,")"))) +
  scale_color_manual(values=c("red","blue"))

ggarrange(jmax_plot3, jmax_plot4, ncol = 2, labels = c("A", "B"))

#gs plots and stats tests ----

gs_plot1 <- ggplot(order_parms, aes(x=Treatment, y=gs, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4)+
  theme(legend.position = "none") +
  ylab(expression(paste("gs (",mol,~ m^2, s^-1,")"))) +
  scale_color_manual(values=c("red","blue")) 

gs_plot2 <- ggplot(order_basic, aes(x=Treatment, y=gs, color = Treatment)) + geom_boxplot() +
  facet_wrap(~Location) + 
  ylab(expression(paste("gs (",mol,~ m^2, s^-1,")"))) +
  scale_color_manual(values=c("red","blue"))

ggarrange(gs_plot1, gs_plot2, ncol = 2, labels = c("A", "B"))

gs_two_way <- aov(gs ~ Location + Treatment, data = parms)
gs_interaction <- aov(gs ~ Treatment + Location*Treatment, data = parms)
gs_nested <- aov(gs ~ Location/Genotype + Treatment, data = parms)

gs_models <- list(gs_two_way, gs_interaction)

aictab(gs_models,modnames = c("two_way", "interaction"))

summary(gs_interaction)

mean_gs_desert_40 <- mean(parms[parms$Location == "Desert" & parms$Treatment == "40",]$gs)
mean_gs_desert_80 <- mean(parms[parms$Location == "Desert" & parms$Treatment == "80",]$gs)
mean_gs_coast_40 <- mean(parms[parms$Location == "Coastal" & parms$Treatment == "40",]$gs)
mean_gs_coast_80 <- mean(parms[parms$Location == "Coastal" & parms$Treatment == "80",]$gs)

#iWUE and stats tests ----

iwue_plot1 <- ggplot(order_parms, aes(x=Treatment, y=iWUE, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4)+
  theme(legend.position = "none") +
  ylab(expression(paste("iWUE (", mu, "mol ", "mol",~ m^2, s^-1,")"))) +
  scale_color_manual(values=c("red","blue")) 

iwue_plot2 <- ggplot(order_basic, aes(x=Treatment, y=iWUE, color = Treatment)) + geom_boxplot() +
  facet_wrap(~Location) + 
  ylab(expression(paste("iWUE (", mu, "mol ", "mol",~ m^2, s^-1,")"))) +
  scale_color_manual(values=c("red","blue"))

ggarrange(iwue_plot1, iwue_plot2, ncol = 2, labels = c("A", "B"))

iwue_two_way <- aov(iWUE ~ Location + Treatment, data = parms)
iwue_interaction <- aov(iWUE ~ Treatment + Treatment*Location, data = parms)
iwue_nested <- aov(iWUE ~ Location/Genotype + Treatment, data = parms)
iwue_both <- aov(iWUE ~ Location/Genotype + Treatment + Treatment*Location, data = parms)

iwue_models <- list(iwue_two_way, iwue_nested, iwue_interaction)
iwue_names <- c("two_way", "nested", "interaction")

aictab(iwue_models, modnames = iwue_names)

summary(iwue_interaction)

mean_iwue_desert_40 <- mean(parms[parms$Location == "Desert" & parms$Treatment == "40",]$iWUE)
mean_iwue_desert_80 <- mean(parms[parms$Location == "Desert" & parms$Treatment == "80",]$iWUE)
mean_iwue_coast_40 <- mean(parms[parms$Location == "Coastal" & parms$Treatment == "40",]$iWUE)
mean_iwue_coast_80 <- mean(parms[parms$Location == "Coastal" & parms$Treatment == "80",]$iWUE)

#sl and stats tests ----

sl_plot1 <- ggplot(order_parms, aes(x=Treatment, y=sl, color=Treatment)) + geom_boxplot() +
  facet_wrap(~Genotype + Location, ncol=4)+
  theme(legend.position = "none") +
  ylab("sl") + 
  scale_color_manual(values=c("red","blue")) 

sl_plot2 <- ggplot(order_basic, aes(x=Treatment, y=sl, color = Treatment)) + geom_boxplot() +
  facet_wrap(~Location) + 
  ylab("sl") +
  scale_color_manual(values=c("red","blue"))

ggarrange(sl_plot1, sl_plot2, ncol = 2, labels = c("A", "B"))

sl_two_way <- aov(sl ~ Location + Treatment, data = parms)
sl_nested <- aov(sl ~ Location/Genotype + Treatment, data = parms)
sl_interaction <- aov(sl ~ Location*Treatment + Treatment, data = parms)

summary(sl_two_way)

mean_sl_desert_40 <- mean(parms[parms$Location == "Desert" & parms$Treatment == "40",]$sl, na.rm = TRUE)
mean_sl_desert_80 <- mean(parms[parms$Location == "Desert" & parms$Treatment == "80",]$sl, na.rm = TRUE)
mean_sl_coast_40 <- mean(parms[parms$Location == "Coastal" & parms$Treatment == "40",]$sl, na.rm = TRUE)
mean_sl_coast_80 <- mean(parms[parms$Location == "Coastal" & parms$Treatment == "80",]$sl, na.rm = TRUE)

#try without the headed plants ----

gs2_two_way <- aov(gs ~ Location + Treatment, data = parms_not_headed) #no difference in stats output
iwue2_two_way <- aov(iWUE ~ Location + Treatment, data = parms_not_headed) #no difference in stats output
sl2_two_way <- aov(sl ~ Location + Treatment, data = parms_not_headed) #no difference in stats output
