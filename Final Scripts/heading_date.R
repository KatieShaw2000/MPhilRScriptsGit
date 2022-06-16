#Set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Phenotyping Campaign")

#Get packages ----

library(ggplot2)
library(gridExtra)
library(tidyr)
library(ggforce)
library(forcats)
library(plyr)
library(lme4)
library(inti)
library(readxl)

#get data ----

dates <- read_excel("HeadingSamplingDates.xlsx")
dates <- select(dates, -Notes, -Aci, -Code, -PN)

#order dataframe ----

dates <- select(dates, Name, PlotID, Rep, Column, Block, `Transplanting-Heading`)
names(dates)[1] <- "Genotype"
dates$Rep <- as.factor(dates$Rep)

#get heading date blues ----

dates_models <- H2cal(data = dates,
                    trait = "`Transplanting-Heading`",
                    gen.name = "Genotype",
                    rep.n = 2,
                    fix.model = "0 + Genotype + (1|Rep) +  (1|Block) + (1|Column)",
                    ran.model = "(1|Genotype) + (1|Rep) + (1|Block) + (1|Column)")

summary(dates_models$model) #all of the variance is above 0 so leave components in!
dates_models$tabsmr
dates_blues <- dates_models$blues
dates_blues <- dates_blues[,c(1,2)]
names(dates_blues)[2] <- "days_to_heading"

#export SLA BLUEs ----

write.csv(dates_blues, "~/OneDrive - University of Cambridge/MPhil/GitLink/Final Scripts Output/heading_date_blues.csv")
