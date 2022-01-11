#Get packages ----

library(readxl)
library(lme4)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(inti)

#get data ----

dates <- read_excel("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/HeadingSamplingDates.xlsx")
dates <- select(dates, "Name", "PlotID", "Rep", "Transplanting-Heading")
names(dates)[4] <- "TransplantingHeading"

hr <- H2cal(data = dates,
            trait = "TransplantingHeading",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep)",
            ran.model = "(1|Name) + (1|Rep)",
            plot_diag=TRUE)

hr$tabsmr
blues <- hr$blues
names(blues)[2] <- "blues"
blups <- hr$blups
names(blups)[2] <- "blups"

blues_blups <- merge(blues, blups, by = "Name")
blues_blups <- blues_blups[,-3]

ggplot(blues_blups, aes(x=blues, y=blups)) + geom_point() + xlab("BLUEs") + ylab("BLUPs")

write.csv(blues_blups, "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/blues blups/TransplantingHeading.csv")

ggplot(blues, aes(x=blues)) + geom_density(color="darkblue", fill="lightblue") + xlab("No. of Days to Heading: BLUEs") +
  ylab("Density") + theme_classic()

ggplot(blups, aes(x=blups)) + geom_density(color="darkblue", fill="lightblue") + xlab("No. of Days to Heading: BLUPs") +
  ylab("Density") + theme_classic()


