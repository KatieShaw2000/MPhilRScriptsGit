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

NPQ <- read_excel("~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/NPQparms.xlsx")

#try some plots for plot means ----

NPQ_means <- aggregate(NPQ[,3:14], list(NPQ$Name, NPQ$Plot, NPQ$Rep), mean)
names(NPQ_means)[1] <- "Name"
names(NPQ_means)[2] <- "Plot"
names(NPQ_means)[3] <- "Rep"

ggplot(NPQ_means, aes(x=a_fit)) + geom_density()
ggplot(NPQ_means, aes(x=b_fit)) + geom_density()
ggplot(NPQ_means, aes(x=c_fit)) + geom_density()
ggplot(NPQ_means, aes(x=d_fit)) + geom_density()
ggplot(NPQ_means, aes(x=e_fit)) + geom_density()
ggplot(NPQ_means, aes(x=f_fit)) + geom_density()
ggplot(NPQ_means, aes(x=g_fit)) + geom_density()
ggplot(NPQ_means, aes(x=h_fit)) + geom_density()
ggplot(NPQ_means, aes(x=max_amp)) + geom_density()
ggplot(NPQ_means, aes(x=end_NPQ)) + geom_density()
ggplot(NPQ_means, aes(x=gradient)) + geom_density()
ggplot(NPQ_means, aes(x=end_FvFm)) + geom_density()

#get blues/blups----

info <- read_excel("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/CF files/NPQ_additional_info.xlsx")
info <- select(info, Plot, Date)
info <- unique(info)

NPQ_means <- aggregate(NPQ[,3:10], list(NPQ$Name, NPQ$Plot, NPQ$Rep), mean)

names(NPQ_means)[1] <- "Name"
names(NPQ_means)[2] <- "Plot"
names(NPQ_means)[3] <- "Rep"

NPQ_means <- merge(NPQ_means, info, by = "Plot")

#a ----

hr <- H2cal(data = NPQ_means,
            trait = "a_fit",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep) + (1|Date)",
            ran.model = "(1|Name) + (1|Rep) + (1|Date)",
            plot_diag = TRUE)

hr$tabsmr
a_blues <- hr$blues
names(a_blues)[2] <- "BLUEs"
a_blups <- hr$blups
names(a_blups)[2] <- "BLUPs"

a_blups_blues <- merge(a_blues, a_blups, by = "Name")
a_blups_blues <- a_blups_blues[,-3]
ggplot(a_blups_blues, aes(x=BLUEs, y=BLUPs)) + geom_point()

#b ----

#c ----

#d ----

#e ----

#max amp ----

#initial slope ----

#end NPQ ----