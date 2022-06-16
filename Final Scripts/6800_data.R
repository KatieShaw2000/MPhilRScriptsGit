#set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/GitLink/Final Scripts Output")

#get packages needed ----

library(readxl)
library(data.table)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(writexl)
library(plantecophys)
library(gridExtra)
library(ggforce)
library(forcats)
library(plyr)
library(lme4)
library(inti)
library(writexl)

#get data ----

ACiDATA <- read_excel("ACi_data.xlsx")

#Fitting ----

names(ACiDATA)[20] <- "Tleaf"
names(ACiDATA)[64] <- "PPFD"
names(ACiDATA)[15] <- "gs"

reduced_df <- select(ACiDATA, Ci, A, gs, Tleaf,PPFD, Plot, Repeat, Rep, Name) #get columns I want
names(reduced_df)[2] <- "Photo"
reduced_df$PlotRepeat <- paste(reduced_df$Plot,reduced_df$Repeat)

reduced_df$Ci <- as.numeric(reduced_df$Ci)
reduced_df$Photo <- as.numeric(reduced_df$Photo)
reduced_df$PPFD <- as.numeric(reduced_df$PPFD)
reduced_df$Tleaf <- as.numeric(reduced_df$Tleaf)
reduced_df$gs <- as.numeric(reduced_df$gs)

write_csv(reduced_df,"ACi_data_reduced.csv")

#load as csv- needs to be read as csv to work for some reason

to_fit <- read.csv("ACi_data_reduced.csv")

#fitting ----

fitting <- fitacis(to_fit, "PlotRepeat", fitmethod = "bilinear", fitTPU=TRUE)
fitting_output <- coef(fitting)
fitting_output <- select(fitting_output, PlotRepeat,Vcmax,Jmax,TPU)
fitting_output <- separate(fitting_output, PlotRepeat, c("Plot", "Repeat"), sep = ' ')

#calculation of additional parms ---- 

asat <- to_fit[!duplicated(to_fit$PlotRepeat),] 
asat <- subset(asat, select = c("PlotRepeat", "Plot","Repeat", "Rep", "Name", "Photo", "gs"))
names(asat)[6] <- "asat"

asat$iWUE <- asat$asat/asat$gs #calculate water use efficiency

output <- vector()

for (i in 1:length(fitting)) {
  x <- fitting[[i]]
  x <- x$Photosyn(Ci=400)$ALEAF
  output <- append(output, x)
}

asat$a400 <- output
asat$sl <- (asat$a400 - asat$asat)/asat$a400 #calculate 
asat <- asat[,-1]
rownames(asat) <- NULL

#merge into one dataframe ----

aci_parms <- merge(fitting_output, asat, by = c("Plot", "Repeat"))
aci_parms <- aci_parms[,c(7,1,2,6,3,4,5,8,9,10,11,12)]

names(aci_parms)[1] <- "Genotype"

#clean global environment ----

rm(ACiDATA, asat, fitting, fitting_output, reduced_df, to_fit, i, output, x)

#adding additional information in for blues calculations ----

licor_info <- read_excel("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Phenotyping Campaign/LICOR 6800 files/additional_aci_information.xlsx")
names(licor_info)[1] <- "Genotype"
licor_info <- licor_info[,-c(7,10)]

hd <- read_excel("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Phenotyping Campaign/HeadingSamplingDates.xlsx") #want heading date too 
hd <- hd[,c(6,2,3,1,9)]
names(hd)[1] <- "Genotype"
names(hd)[4] <- "Plot"

info_parms <- merge(aci_parms, licor_info, by = c("Genotype", "Plot", "Repeat", "Rep"))
info_parms <- merge(info_parms, hd, by = c("Genotype", "Plot"))

#clean global environment ----

rm(hd, licor_info, aci_parms)

#remove those with crazily high jmax fitted ----

info_parms <- subset(info_parms, Jmax <= 300)

#Jmax blues ----

jmax_models <- H2cal(data = info_parms,
                  trait = "Jmax",
                  gen.name = "Genotype",
                  rep.n = 6,
                  fix.model = "0 + Genotype + (1|Column) + (1|Block) + (1|LICOR_user) + (1|LICOR_start_hour) + (1|LICOR_ID)",
                  ran.model = "(1|Genotype) + (1|Column) + (1|Block) + (1|LICOR_user) + (1|LICOR_start_hour) + (1|LICOR_ID)")

summary(jmax_models$model) #remove licor date, heading date and rep as these don't contribute to the variation
jmax_models$tabsmr
jmax_blues <- jmax_models$blues
jmax_blues <- jmax_blues[,c(1,2)]

#vcmax blues ----

vcmax_models <- H2cal(data = info_parms,
                     trait = "Vcmax",
                     gen.name = "Genotype",
                     rep.n = 6,
                     fix.model = "0 + Genotype + (1|Column) + (1|Block) + (1|LICOR_date) + (1|LICOR_start_hour) + (1|LICOR_ID)",
                     ran.model = "(1|Genotype) + (1|Column) + (1|Block) + (1|LICOR_date) + (1|LICOR_start_hour) + (1|LICOR_ID)")

summary(vcmax_models$model) 
vcmax_models$tabsmr
vcmax_blues <- vcmax_models$blues
vcmax_blues <- vcmax_blues[,c(1,2)]

#asat models ----

asat_models <- H2cal(data = info_parms,
                     trait = "asat",
                     gen.name = "Genotype",
                     rep.n = 6,
                     fix.model = "0 + Genotype + (1|Column) + (1|Block) + (1|Heading) + (1|LICOR_start_hour) + (1|LICOR_ID)",
                     ran.model = "(1|Genotype) + (1|Column) + (1|Block) + (1|Heading) + (1|LICOR_start_hour) + (1|LICOR_ID)")

summary(asat_models$model) 
asat_models$tabsmr
asat_blues <- asat_models$blues
asat_blues <- asat_blues[,c(1,2)]

#gs models ----

gs_models <- H2cal(data = info_parms,
                      trait = "gs",
                      gen.name = "Genotype",
                      rep.n = 6,
                      fix.model = "0 + Genotype + (1|Column) + (1|Block) + (1|Heading) + (1|LICOR_date) + (1|LICOR_start_hour) + (1|LICOR_ID)",
                      ran.model = "(1|Genotype) + (1|Column) + (1|Block) + (1|Heading) + (1|LICOR_date) + (1|LICOR_start_hour) + (1|LICOR_ID)")

summary(gs_models$model) 
gs_models$tabsmr
gs_blues <- gs_models$blues
gs_blues <- gs_blues[,c(1,2)]

#iwue models ---- 

iwue_models <- H2cal(data = info_parms,
                   trait = "iWUE",
                   gen.name = "Genotype",
                   rep.n = 6,
                   fix.model = "0 + Genotype + (1|Rep) + (1|Column) + (1|Block) + (1|Heading) + (1|LICOR_user) + (1|LICOR_ID)",
                   ran.model = "(1|Genotype) + (1|Rep) + (1|Column) + (1|Block) + (1|Heading) + (1|LICOR_user) + (1|LICOR_ID)")

summary(iwue_models$model) 
iwue_models$tabsmr
iwue_blues <- iwue_models$blues
iwue_blues <- iwue_blues[,c(1,2)]

#sl models ---- 

sl_models <- H2cal(data = info_parms,
                     trait = "sl",
                     gen.name = "Genotype",
                     rep.n = 6,
                     fix.model = "0 + Genotype + (1|Rep) + (1|Column) + (1|Heading) + (1|LICOR_user) + (1|LICOR_ID)",
                     ran.model = "(1|Genotype) + (1|Rep) + (1|Column) + (1|Heading) + (1|LICOR_user) + (1|LICOR_ID)")

summary(sl_models$model) 
sl_models$tabsmr
sl_blues <- sl_models$blues
sl_blues <- sl_blues[,c(1,2)]

#merge all the blues into one dataframe --- 

aci_blues <- list(jmax_blues, vcmax_blues, asat_blues, gs_blues, iwue_blues, sl_blues) %>%
  reduce(left_join, by = "Genotype")

#export blues ---- 

write.csv(aci_blues, "aci_blues.csv")
