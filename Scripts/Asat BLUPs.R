#get libraries ----

library(readxl)
library(lme4)
library(desplot)
library(emmeans)
library(ggplot2)
library(readxl)
library(tidyverse)
library(inti)

#get data ----

data <- read_excel("~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/6400dataforblups.xlsx")

#averages based on plots ----

means <- aggregate(data[,5], list(data$Name, data$Plot, data$Rep, data$Date, data$Block, data$Column), mean)
names(means)[1] <- "Name"
names(means)[2] <- "Plot"
names(means)[3] <- "Rep"
names(means)[4] <- "Date"
names(means)[5] <- "Block"
names(means)[6] <- "Column"


#get blups ----

hr <- H2cal(data = means,
            trait = "mean_Asat",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep) + (1|Date) + (1|Block) + (1|Column)",
            ran.model = "(1|Name) + (1|Rep) + (1|Date) + (1|Block) + (1|Column)",
            plot_diag = TRUE)

hr$tabsmr
asat_blups <- hr$blups
names(asat_blups)[2] <- "asat_blups"

ggplot(asat_blups, aes(x=asat_blups)) + geom_density(color="darkblue", fill="lightblue") + xlab("Asat BLUPs") +
  ylab("Density") + theme_classic()


write.csv(asat_blups, "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/BLUPs for PCA/Asat_blups.csv")
