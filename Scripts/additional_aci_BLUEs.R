#get libraries 

library(readxl)
library(lme4)
library(desplot)
library(emmeans)
library(ggplot2)
library(readxl)
library(tidyverse)
library(inti)

#get data ----

data <- read_excel("~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/extra_aci_parms.xlsx")

#get averages ----

means <- aggregate(data[,5:9], list(data$Name, data$Plot, data$Rep), mean)
names(means)[1] <- "Name"
names(means)[2] <- "Plot"
names(means)[3] <- "Rep"

#get blups for iWUE ----

hr <- H2cal(data = means,
            trait = "iWUE",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep)",
            ran.model = "(1|Name) + (1|Rep)",
            plot_diag = TRUE)

hr$tabsmr
iWUE_blups <- hr$blups
names(iWUE_blups)[2] <- "iWUE"

ggplot(iWUE_blups, aes(x=iWUE)) + geom_density(color="darkblue", fill="lightblue") + xlab("iWUE BLUPs") +
  ylab("Density") + theme_classic()

#get blups for sl ----

hr <- H2cal(data = means,
            trait = "sl",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep)",
            ran.model = "(1|Name) + (1|Rep)",
            plot_diag = TRUE)

hr$tabsmr
sl_blups <- hr$blups
names(sl_blups)[2] <- "sl"

ggplot(sl_blups, aes(x=sl)) + geom_density(color="darkblue", fill="lightblue") + xlab("sl BLUPs") +
  ylab("Density") + theme_classic()

#get blups for gs ----

hr <- H2cal(data = means,
            trait = "gs",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep)",
            ran.model = "(1|Name) + (1|Rep)",
            plot_diag = TRUE)

hr$tabsmr
gs_blups <- hr$blups
names(gsl_blups)[2] <- "gs"

ggplot(gs_blups, aes(x=gs)) + geom_density(color="darkblue", fill="lightblue") + xlab("gs BLUPs") +
  ylab("Density") + theme_classic()

#get blups for asat ----

hr <- H2cal(data = means,
            trait = "asat",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep)",
            ran.model = "(1|Name) + (1|Rep)",
            plot_diag = TRUE)

hr$tabsmr
asat_blups <- hr$blups
names(asat_blups)[2] <- "asat"

ggplot(asat_blups, aes(x=asat)) + geom_density(color="darkblue", fill="lightblue") + xlab("slasat BLUPs") +
  ylab("Density") + theme_classic()

#merge into one ----

additional_aci_blups <- merge(iWUE_blups, sl_blups, by = "Name")
additional_aci_blups <- merge(additional_aci_blups, gs_blups, by = "Name")
additional_aci_blups <- merge(additional_aci_blups, asat_blups, by = "Name")

#export dataframe ----

write.csv(additional_aci_blups,
          "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/BLUPs for PCA/additional_aci_blups.csv")

