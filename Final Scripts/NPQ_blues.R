#set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/GitLink/Final Scripts Output")

#Get packages ----

library(readxl)
library(lme4)
library(desplot)
library(emmeans)
library(ggplot2)
library(readxl)
library(tidyverse)
library(inti)

#get data ----

parms <- read_excel("NPQ_parms.xlsx")
names(parms)[1] <- "Genotype"
parms$Repeat <- as.factor(parms$Repeat)

#get means for reps ----

NPQ_means <- aggregate(parms[,7:18], list(parms$Genotype, parms$Plot, parms$Rep, parms$Block, parms$Column), mean)

names(NPQ_means)[1] <- "Genotype"
names(NPQ_means)[2] <- "Plot"
names(NPQ_means)[3] <- "Rep"
names(NPQ_means)[4] <- "Block"
names(NPQ_means)[5] <- "Column"

#get some additional info on sampling date and heading date ----

info <- read_excel("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/CF files/NPQ_additional_info.xlsx")
info <- select(info, Plot, Date)
info <- unique(info)

NPQ_means <- merge(NPQ_means, info, by = "Plot")

hd <- read_excel("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Phenotyping Campaign/HeadingSamplingDates.xlsx") #want heading date too 
hd <- hd[,c(6,1,9)]
names(hd)[1] <- "Genotype"
names(hd)[2] <- "Plot"

NPQ_means <- merge(NPQ_means, hd, by = c("Genotype", "Plot"))

rm(parms, hd, info) #to avoid too many things in global environment

#a blues ---- 

a_models <- H2cal(data = NPQ_means,
            trait = "a_fit",
            gen.name = "Genotype",
            rep.n = 2,
            fix.model = "0 + Genotype + (1|Rep) + (1|Column) + (1|Block) + (1|Heading) + (1|Date)",
            ran.model = "(1|Genotype) + (1|Rep) + (1|Column) + (1|Block) + (1|Heading) + (1|Date)")

summary(a_models$model) #all contribute variance so keep in
a_models$tabsmr
a_blues <- a_models$blues
a_blues <- a_blues[,c(1,2)]

#b blues ----

b_models <- H2cal(data = NPQ_means,
                  trait = "b_fit",
                  gen.name = "Genotype",
                  rep.n = 2,
                  fix.model = "0 + Genotype + (1|Rep) + (1|Column) + (1|Block) + (1|Heading) + (1|Date)",
                  ran.model = "(1|Genotype) + (1|Rep) + (1|Column) + (1|Block) + (1|Heading) + (1|Date)")

summary(b_models$model) #all contribute variance so keep in
b_models$tabsmr
b_blues <- b_models$blues
b_blues <- b_blues[,c(1,2)]

#c blues ----

c_models <- H2cal(data = NPQ_means,
                  trait = "c_fit",
                  gen.name = "Genotype",
                  rep.n = 2,
                  fix.model = "0 + Genotype + (1|Column) + (1|Block) + (1|Heading) + (1|Date)",
                  ran.model = "(1|Genotype) + (1|Column) + (1|Block) + (1|Heading) + (1|Date)")

summary(c_models$model) #rep doesn't contribute any variance so remove from models
c_models$tabsmr
c_blues <- c_models$blues
c_blues <- c_blues[,c(1,2)]

#d blues ----

d_models <- H2cal(data = NPQ_means,
                  trait = "d_fit",
                  gen.name = "Genotype",
                  rep.n = 2,
                  fix.model = "0 + Genotype + (1|Column) + (1|Block) + (1|Heading) + (1|Date)",
                  ran.model = "(1|Genotype) + (1|Column) + (1|Block) + (1|Heading) + (1|Date)")

summary(d_models$model) #rep doesn't contribute any variance so remove from models
d_models$tabsmr
d_blues <- d_models$blues
d_blues <- d_blues[,c(1,2)]

#e blues ----

e_models <- H2cal(data = NPQ_means,
                  trait = "e_fit",
                  gen.name = "Genotype",
                  rep.n = 2,
                  fix.model = "0 + Genotype + (1|Column) + (1|Block) + (1|Heading) + (1|Date)",
                  ran.model = "(1|Genotype) + (1|Column) + (1|Block) + (1|Heading) + (1|Date)")

summary(e_models$model) #rep doesn't contribute any variance so remove from models
e_models$tabsmr
e_blues <- e_models$blues
e_blues <- e_blues[,c(1,2)]

#f blues ----

f_models <- H2cal(data = NPQ_means,
                  trait = "f_fit",
                  gen.name = "Genotype",
                  rep.n = 2,
                  fix.model = "0 + Genotype + (1|Rep) + (1|Column) + (1|Block) + (1|Heading) + (1|Date)",
                  ran.model = "(1|Genotype) + (1|Rep) + (1|Column) + (1|Block) + (1|Heading) + (1|Date)")

summary(f_models$model) #all contribute to variance so keep in 
f_models$tabsmr
f_blues <- f_models$blues
f_blues <- f_blues[,c(1,2)]

#g blues ----

g_models <- H2cal(data = NPQ_means,
                  trait = "g_fit",
                  gen.name = "Genotype",
                  rep.n = 2,
                  fix.model = "0 + Genotype + (1|Rep) + (1|Column) + (1|Block) + (1|Heading) + (1|Date)",
                  ran.model = "(1|Genotype) + (1|Rep) + (1|Column) + (1|Block) + (1|Heading) + (1|Date)")

summary(g_models$model) #all contribute to variance so keep in 
g_models$tabsmr
g_blues <- g_models$blues
g_blues <- g_blues[,c(1,2)]

#h blues ----

h_models <- H2cal(data = NPQ_means,
                  trait = "h_fit",
                  gen.name = "Genotype",
                  rep.n = 2,
                  fix.model = "0 + Genotype + (1|Rep) + (1|Column) + (1|Block) + (1|Heading) + (1|Date)",
                  ran.model = "(1|Genotype) + (1|Rep) + (1|Column) + (1|Block) + (1|Heading) + (1|Date)")

summary(h_models$model) #all contribute to variance so keep in 
h_models$tabsmr
h_blues <- h_models$blues
h_blues <- h_blues[,c(1,2)]

#max amp blues ----

max_amp_models <- H2cal(data = NPQ_means,
                  trait = "max_amp",
                  gen.name = "Genotype",
                  rep.n = 2,
                  fix.model = "0 + Genotype + (1|Column) + (1|Block) + (1|Heading) + (1|Date)",
                  ran.model = "(1|Genotype) + (1|Column) + (1|Block) + (1|Heading) + (1|Date)")

summary(max_amp_models$model) #rep doesn't contribute to variance so remove
max_amp_models$tabsmr
max_amp_blues <- max_amp_models$blues
max_amp_blues <- max_amp_blues[,c(1,2)]

#initial gradient blues ----

gradient_models <- H2cal(data = NPQ_means,
                        trait = "gradient",
                        gen.name = "Genotype",
                        rep.n = 2,
                        fix.model = "0 + Genotype + (1|Rep) + (1|Column) + (1|Block) + (1|Heading) + (1|Date)",
                        ran.model = "(1|Genotype) + (1|Rep) + (1|Column) + (1|Block) + (1|Heading) + (1|Date)")

summary(gradient_models$model) #all contribute to variance so keep in 
gradient_models$tabsmr
gradient_blues <- gradient_models$blues
gradient_blues <- gradient_blues[,c(1,2)]

#end NPQ blues ---- 

end_NPQ_models <- H2cal(data = NPQ_means,
                         trait = "end_NPQ",
                         gen.name = "Genotype",
                         rep.n = 2,
                         fix.model = "0 + Genotype + (1|Rep) + (1|Column) + (1|Block) + (1|Heading) + (1|Date)",
                         ran.model = "(1|Genotype) + (1|Rep) + (1|Column) + (1|Block) + (1|Heading) + (1|Date)")

summary(end_NPQ_models$model) #all contribute to variance so keep in 
end_NPQ_models$tabsmr
end_NPQ_blues <- end_NPQ_models$blues
end_NPQ_blues <- end_NPQ_blues[,c(1,2)]

#end Fv/Fm blues ----

end_fvfm_models <- H2cal(data = NPQ_means,
                        trait = "end_FvFm",
                        gen.name = "Genotype",
                        rep.n = 2,
                        fix.model = "0 + Genotype + (1|Rep) + (1|Column) + (1|Block) + (1|Heading) + (1|Date)",
                        ran.model = "(1|Genotype) + (1|Rep) + (1|Column) + (1|Block) + (1|Heading) + (1|Date)")

summary(end_fvfm_models$model) #all contribute to variance so keep in 
end_fvfm_models$tabsmr
end_fvfm_blues <- end_fvfm_models$blues
end_fvfm_blues <- end_fvfm_blues[,c(1,2)]

#merge all blues into one dataframe and export ----

NPQ_blues <- list(a_blues, b_blues, c_blues, d_blues, e_blues, f_blues, g_blues,
                  h_blues, end_NPQ_blues, max_amp_blues, gradient_blues, end_fvfm_blues) %>%
                    reduce(left_join, by = "Genotype")

write.csv(NPQ_blues, "NPQ_blues.csv")
