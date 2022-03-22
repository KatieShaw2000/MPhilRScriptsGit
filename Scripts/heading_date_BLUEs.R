#Get packages ----

library(readxl)
library(lme4)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(inti)

#get data ----

dates <- read_excel("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/HeadingSamplingDates.xlsx")
dates <- select(dates, "Name", "PlotID", "Rep", "Transplanting-Heading", "Column", "Block")
names(dates)[4] <- "TransplantingHeading"

hr <- H2cal(data = dates,
            trait = "TransplantingHeading",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep) + (1|Block) + (1|Column)",
            ran.model = "1 + (1|Name) + (1|Rep)+ (1|Block) + (1|Column)",
            plot_diag=TRUE)

hr$tabsmr
blues <- hr$blues
names(blues)[2] <- "blues"
blups <- hr$blups
names(blups)[2] <- "heading_date_blups"

blues_blups <- merge(blues, blups, by = "Name")
blues_blups <- blues_blups[,-3]

ggplot(blues_blups, aes(x=blues, y=blups)) + geom_point() + xlab("BLUEs") + ylab("BLUPs")

write.csv(blups, "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/BLUPs for PCA/heading_date_blups.csv")

ggplot(blues, aes(x=blues)) + geom_density(color="darkblue", fill="lightblue") + xlab("No. of Days to Heading: BLUEs") +
  ylab("Density") + theme_classic()

ggplot(blups, aes(x=heading_date_blups)) + geom_density(color="darkblue", fill="lightblue") + xlab("No. of Days to Heading: BLUPs") +
  ylab("Density") + theme_classic()

