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

#let's try some plots----

order_parms <- within(parms, Genotype <- factor(Genotype, levels=c("B1K-05-12", "B1K-05-08",
                                                                   "B1K-12-10",
                                                                   "B1K-04-03",
                                                                   "B1K-03-17",
                                                                   "B1K-17-17",
                                                                   "B1K-10-01",
                                                                   "B1K-49-10")))

ggplot(order_parms, aes(x=Treatment, y=Jmax, colour=Treatment)) + 
  ylab("Jmax")+
  xlab("Treatment")+
  geom_boxplot() + 
  facet_wrap(~Genotype+Location, ncol=4)


#try some two-way anovas --> actually don't think this is the way to go!! ----

jmax_no_interaction <- aov(Jmax ~ Genotype + Treatment, data = parms)
jmax_with_interaction <- aov(Jmax ~ Genotype*Treatment, data = parms)

model.set <- list(jmax_no_interaction, jmax_with_interaction)
model.names <- c("no_int", "int")
aictab(model.set, modnames = model.names)

summary(jmax_no_interaction)
TukeyHSD(jmax_no_interaction)

#some stats tests! ----

B1K_05_12_40 <- subset(parms, Genotype == "B1K-05-12" & Treatment == 40)
B1K_05_12_80 <- subset(parms, Genotype == "B1K-05-12" & Treatment == 80)

t.test(B1K_05_12_40$Jmax, B1K_05_12_80$Jmax, var.equal = T)
t.test(B1K_05_12_40$Vcmax, B1K_05_12_80$Vcmax, var.equal = T)
t.test(B1K_05_12_40$asat, B1K_05_12_80$asat, var.equal = T)
t.test(B1K_05_12_40$sl, B1K_05_12_80$sl, var.equal = T)
t.test(B1K_05_12_40$iWUE, B1K_05_12_80$iWUE, var.equal = T)
t.test(B1K_05_12_40$gs, B1K_05_12_80$gs, var.equal = T)

B1K_05_08_40 <- subset(parms, Genotype == "B1K-05-08" & Treatment == 40)
B1K_05_08_80 <- subset(parms, Genotype == "B1K-05-08" & Treatment == 80)

t.test(B1K_05_08_40$Jmax, B1K_05_08_80$Jmax, var.equal = T)
t.test(B1K_05_08_40$Vcmax, B1K_05_08_80$Vcmax, var.equal = T)
t.test(B1K_05_08_40$asat, B1K_05_08_80$asat, var.equal = T)
t.test(B1K_05_08_40$gs, B1K_05_08_80$gs, var.equal = T)
t.test(B1K_05_08_40$iWUE, B1K_05_08_80$iWUE, var.equal = T)
t.test(B1K_05_08_40$sl, B1K_05_08_80$sl, var.equal = T)
