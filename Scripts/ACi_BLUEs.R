#get libraries ----

library(readxl)
library(lme4)
library(desplot)
library(emmeans)
library(ggplot2)
library(readxl)
library(tidyverse)
library(inti)

#get data (and plot means) ----

parms <- read_csv("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/TPUACi.csv")
parms <- parms[,-1]
parms <- separate(parms, PlotRepeat, c("Plot", "Repeat"))

parms <- parms[-c(18,50,225,235),]


genotypes <- read_excel("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files/ACiCoefWithLICORs.xlsx")
genotypes <- select(genotypes, Name, Plot, Rep)
genotypes <- unique(genotypes)
genotypes <- genotypes[-91,]

parms <- merge(genotypes, parms, by = "Plot")

ggplot(parms, aes(x=reorder(Name, TPU_Vcmax, FUN = median), y=TPU_Vcmax)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Genotype") + ylab("Vcmax (Raw)")
  
ggplot(parms, aes(x=reorder(Name, TPU_Jmax, FUN = median), y=TPU_Jmax)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Genotype") + ylab("Jmax (Raw)")

ggplot(parms, aes(x=TPU_Jmax, y=TPU_Vcmax)) + geom_point()

parms_means <- aggregate(parms[,c(5,6)], list(parms$Plot), mean)
names(parms_means)[1] <- "Plot"

LICOR_ACI <- read_excel("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files/ACiCoefWithLICORs.xlsx")
LICOR_ACI <- select(LICOR_ACI,-Repeat, -Jmax, -Vcmax)
LICOR_ACI <- LICOR_ACI[1:243,]
LICOR_ACI <- LICOR_ACI[,-9]

#get BLUPs without any LICOR information 

parm_means <- aggregate(parms[,5:6], list(parms$Name,parms$Rep), mean)
names(parm_means)[1]<-"Name"
names(parm_means)[2]<- "Rep"

hr_vcmax <- H2cal(data = parm_means,
            trait = "TPU_Vcmax",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep)",
            ran.model = "(1|Name) + (1|Rep)")

hr_vcmax$tabsmr
vcmax_blups <- hr_vcmax$blups

ggplot(vcmax_blups, aes(x=TPU_Vcmax)) + geom_density(color="darkblue", fill="lightblue") + xlab("Vcmax: BLUPs") +
  ylab("Density") + theme_classic()


hr_jmax <- H2cal(data = parm_means,
                  trait = "TPU_Jmax",
                  gen.name = "Name",
                  rep.n = 2,
                  fix.model = "0 + Name + (1|Rep)",
                  ran.model = "(1|Name) + (1|Rep)")

hr_jmax$tabsmr
jmax_blups <- hr_jmax$blups

ggplot(jmax_blups, aes(x=TPU_Jmax)) + geom_density(color="darkblue", fill="lightblue") + xlab("Jmax: BLUPs") +
  ylab("Density") + theme_classic()

aci_blups <- merge(vcmax_blups, jmax_blups, by = "Name")

ggplot(aci_blups, aes(x=TPU_Vcmax, y=TPU_Jmax)) + geom_point()

write.csv(aci_blups, "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/BLUPs for PCA/aci_blups.csv")
