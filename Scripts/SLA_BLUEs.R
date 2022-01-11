#set working directory ----

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/SLA Data")

#get libraries ----

library(lme4)
library(desplot)
library(emmeans)
library(ggplot2)
library(readxl)
library(tidyverse)
library(inti)

#get data 

SLA <- read_csv("SLA.csv")

#get averages of SLA for each plot ----

SLA_means <- aggregate(SLA[,7], list(SLA$Name,SLA$Plot,SLA$Rep, SLA$Column, SLA$Block,
                                     SLA$Heading_date, SLA$sampling_date), mean)
names(SLA_means)[1] <- "Name"
names(SLA_means)[2] <- "Plot"
names(SLA_means)[3] <- "Rep"
names(SLA_means)[4] <- "Column"
names(SLA_means)[5] <- "Block"
names(SLA_means)[6] <- "Heading_date"
names(SLA_means)[7] <- "Sampling_date"

SLA_means <- SLA_means[order(SLA_means$Name),]

#make a linear model without any spatial element with genotype as fixed effect ----

SLA_model <- lmer(data = SLA_means, SLA ~ Name + (1|Rep) + (1|Sampling_date) + (1|Heading_date))
summary(SLA_model)

#get BLUEs ----

blues <- emmeans(SLA_model, "Name")
blues_df <- summary(blues)

SLA_emmean <- merge(SLA_means, blues_df, by = "Name")

ggplot(SLA_emmean, aes(x=SLA, y=emmean)) + geom_point() +
  xlim(125,350) + ylim(125,350)

names(SLA_emmean)[9] <- "BLUEs"

#calculating heritability ----

hr <- H2cal(data = SLA_means,
            trait = "SLA",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep) + (1|Sampling_date) + (1|Heading_date)",
            ran.model = "(1|Name) + (1|Rep) + (1|Sampling_date) + (1|Heading_date)")

hr$tabsmr
hr$blues
hr$blups
summary(hr)
summary(hr$model)

SLA_blues <- hr$blues
names(SLA_blues)[2] <- "BLUEs"
SLA_blups <- hr$blups
names(SLA_blups)[2] <- "BLUPs"

ggplot(SLA_blues, aes(x=BLUEs)) + geom_density(color="darkblue", fill="lightblue") + xlab("SLA: BLUEs") +
  ylab("Density") + theme_classic()

ggplot(SLA_blups, aes(x=BLUPs)) + geom_density(color="darkblue", fill="lightblue") + xlab("SLA: BLUPs") +
  ylab("Density") + theme_classic()

ggplot(SLA_blues, aes(x=reorder(Name, BLUEs), y = BLUEs)) + geom_point()

SLA_blues_blups <- merge(SLA_blues, SLA_blups, by = "Name")
SLA_blues_blups <- SLA_blues_blups[,-3]

ggplot(SLA_blues_blups, aes(x=BLUEs, y=BLUPs)) + geom_point()

write.csv(SLA_blues_blups, "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/blues blups/SLA.csv")

#compare to BLUPs ----

SLA_model_random <- lmer(data=SLA_means, SLA ~ (1|Name) + (1|Rep) + (1|Sampling_date) + (1|Heading_date))
summary(SLA_model_random)

SLA_blups <- ranef(SLA_model_random)
SLA_blups_genotypes <- SLA_blups$Name
SLA_blups_genotypes$Name <- rownames(SLA_blups_genotypes)
rownames(SLA_blups_genotypes) <- NULL
colnames(SLA_blups_genotypes) <- c("SLA", "Name")
SLA_blups_genotypes$adjusted_SLA <- SLA_blups_genotypes$SLA + mean(SLA$SLA, na.rm = TRUE)

compare <- merge(SLA_emmean, SLA_blups_genotypes, by = "Name")

ggplot(compare, aes(x=BLUEs, y = adjusted_SLA)) + geom_point() +
  xlim(160,300) + ylim(160,300) + ylab("BLUPs")

#----
#var_SLA <- as.data.frame(VarCorr(SLA_model, comp = "vcov"))
#head(var_SLA)

#h_SLA <- var_SLA[1,4]/sum(var_SLA[,4])

#residuals <- residuals(SLA_model)
#resisuals <- as.data.frame(residuals)
#new_SLA <- data.frame(SLA_means, residuals)

#plot residuals to see if there seems to be a spatial pattern ----

#rep1 <- subset(new_SLA, Rep == "1")
#rep2 <- subset(new_SLA, Rep == "2")

#SLA_grid1 <- desplot(rep1, SLA~Block*Column)
#SLA_grid2 <- desplot(rep2, SLA~Block*Column)

#residuals_grid1 <- desplot(rep1, residuals~Block*Column)
#residuals_grid2 <- desplot(rep2, residuals~Block*Column)

#get BLUEs ----

