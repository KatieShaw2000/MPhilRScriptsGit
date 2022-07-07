#set working directory ----

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/Cleaned 6400 files")

#get packages needed ----

library(readxl)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(ggforce)
library(data.table)
library(forcats)
library(plyr)
library(lme4)
library(inti)
library(writexl)
library(tidyverse)

#get data ----

file.list <- list.files(pattern='*.xlsx', recursive = TRUE)
LICOR_6400.list <- lapply(file.list, read_excel)
ACi_6400 <- rbindlist(LICOR_6400.list, fill = TRUE)
ACi_6400$Repeat <- as.factor(ACi_6400$Repeat)

rm(file.list, LICOR_6400.list)

#remove data based on visual inspection of plots ----

ACi_6400$PlotRepeat <- paste(ACi_6400$Plot, ACi_6400$Repeat)

to_remove <- c("1012 2", "1026 2", "1062 1", "1129 1", "1148 2", "1167 2", "1173 1", "1199 2",
               "1216 1", "1222 2", "1254 1", "1263 1", "1318 3", "2015 3", "2028 1", "2031 3", 
               "2036 3", "2045 1", "2051 3", "2082 3", "2100 2", "2103 3", "2122 2", "2135 1",
               "2164 2", "2169 1", "2191 2", "2333 3", "2241 3", "2242 2", "2275 3")

ACi_6400 <- ACi_6400[!ACi_6400$PlotRepeat %in% to_remove,]

rm(to_remove)

# get averages of last 10 points before end of light phase ----

before_drop <- filter(ACi_6400, Light == "1800")
before_drop <- filter(before_drop, Elapsed_time >= 372) #get last minute

after_drop <- filter(ACi_6400, Light == "200")
after_drop <- filter(after_drop, Elapsed_time >= 1020) #get last minute

mean_Asat <- ddply(before_drop, .(Plot, Repeat), summarise, mean_Asat = mean(Photo, na.rm = TRUE)) 
mean_Alow <- ddply(after_drop, .(Plot, Repeat), summarise, mean_Alow = mean(Photo, na.rm = TRUE)) 

mean_gs_Asat <- ddply(before_drop, .(Plot, Repeat), summarise, mean_gs_Asat = mean(Cond, na.rm = TRUE)) 
mean_gs_Alow <- ddply(after_drop, .(Plot, Repeat), summarise, mean_gs_Alow = mean(Cond, na.rm = TRUE)) 

#add in information needed for BLUEs ----

fielddesign <- read_xlsx("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/FieldDesign.xlsx")
fielddesign <- fielddesign[,c(6,1,2,3,5)]
names(fielddesign)[1] <- "Genotype"
names(fielddesign)[2] <- "Plot"

other_info <- select(ACi_6400, "Date", "Start_Hour", "CAP", "Plot", "Repeat")
other_info <- unique(other_info) #remove duplicates

hd <- read_excel("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/Phenotyping Campaign/HeadingSamplingDates.xlsx") #want heading date too 
hd <- hd[,c(6,1,9)]
names(hd)[1] <- "Genotype"
names(hd)[2] <- "Plot"
  
additional_info_blues <- merge(other_info, fielddesign, by = "Plot")
additional_info_blues <- merge(hd, additional_info_blues, by = c("Genotype", "Plot"))

#try blues for mean_Alow ----

mean_Alow_blues_data <- merge(mean_Alow, additional_info_blues, by = c("Plot", "Repeat"))

Alow_models <- H2cal(data = mean_Alow_blues_data,
                      trait = "mean_Alow",
                      gen.name = "Genotype",
                      rep.n = 6,
                      fix.model = "0 + Genotype + (1|Rep) + (1|CAP) + (1|Start_Hour) + (1|Date)",
                      ran.model = "(1|Genotype) + (1|Rep) + (1|CAP) + (1|Start_Hour) + (1|Date)")

summary(Alow_models$model) #block, column and heading were all 0 so remove these from the models 
Alow_models$tabsmr
Alow_blues <- Alow_models$blues
Alow_blues <- Alow_blues[,c(1,2)]

#try blues for mean_Asat ----

mean_Asat_blues_data <- merge(mean_Asat, additional_info_blues, by = c("Plot", "Repeat"))

Asat_models <- H2cal(data = mean_Asat_blues_data,
                     trait = "mean_Asat",
                     gen.name = "Genotype",
                     rep.n = 6,
                     fix.model = "0 + Genotype + (1|Rep) + (1|CAP) + (1|Start_Hour) + (1|Date) + (1|Column) + (1|Block)",
                     ran.model = "(1|Genotype) + (1|Rep) + (1|CAP) + (1|Start_Hour) + (1|Date) + (1|Column) + (1|Block)")

summary(Asat_models$model) #heading was 0 so remove these from the models 
Asat_models$tabsmr 
Asat_blues <- Asat_models$blues
Asat_blues <- Asat_blues[,c(1,2)]

#try blues for mean_gs_Alow ----

mean_gs_Alow_blues_data <- merge(mean_gs_Alow, additional_info_blues, by = c("Plot", "Repeat"))

gs_Alow_models <- H2cal(data = mean_gs_Alow_blues_data,
                     trait = "mean_gs_Alow",
                     gen.name = "Genotype",
                     rep.n = 6,
                     fix.model = "0 + Genotype + (1|Rep) + (1|CAP) + (1|Start_Hour) + (1|Date) + (1|Column) + (1|Block) + (1|Heading)",
                     ran.model = "(1|Genotype) + (1|Rep) + (1|CAP) + (1|Start_Hour) + (1|Date) + (1|Column) + (1|Block) + (1|Heading)")

summary(gs_Alow_models$model) 
gs_Alow_models$tabsmr
gs_Alow_blues <- gs_Alow_models$blues
gs_Alow_blues <- gs_Alow_blues[,c(1,2)]

#try blues for mean_gs_Asat

mean_gs_Asat_blues_data <- merge(mean_gs_Asat, additional_info_blues, by = c("Plot", "Repeat"))

gs_Asat_models <- H2cal(data = mean_gs_Asat_blues_data,
                        trait = "mean_gs_Asat",
                        gen.name = "Genotype",
                        rep.n = 6,
                        fix.model = "0 + Genotype + (1|Rep) + (1|CAP) + (1|Start_Hour) + (1|Date) + (1|Column) + (1|Block) + (1|Heading)",
                        ran.model = "(1|Genotype) + (1|Rep) + (1|CAP) + (1|Start_Hour) + (1|Date) + (1|Column) + (1|Block) + (1|Heading)")

summary(gs_Asat_models$model) 
gs_Asat_models$tabsmr
gs_Asat_blues <- gs_Asat_models$blues
gs_Asat_blues <- gs_Asat_blues[,c(1,2)]

#all 6400 blues to export ----

all_6400_blues <- merge(Asat_blues, gs_Asat_blues, by = "Genotype")
all_6400_blues <- merge(all_6400_blues, Alow_blues, by = "Genotype")
all_6400_blues <- merge(all_6400_blues, gs_Alow_blues, by = "Genotype")

#export data ----

write.csv(all_6400_blues, "~/OneDrive - University of Cambridge/MPhil/GitLink/Final Scripts Output/6400_blues.csv")
