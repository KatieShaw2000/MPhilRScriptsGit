#Get packages ----

library(readxl)
library(data.table)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(writexl)
library(plantecophys)
library(gridExtra)
library(ggforce)
library(forcats)
library(plyr)
library(lme4)
library(inti)
library(writexl)

#Set working directory ----

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files")

#Get data ----

to_fit <- read.csv("reduced_df.csv")

#fit ----

fitting <- fitacis(to_fit, "PlotRepeat", fitmethod = "bilinear", fitTPU=TRUE)
fitting_output <- coef(fitting)
fitting_output <- select(fitting_output, PlotRepeat,Vcmax,Jmax,TPU)
fitting_output <- separate(fitting_output, PlotRepeat, c("Plot", "Repeat"), sep = ' ')

asat <- to_fit[!duplicated(to_fit$PlotRepeat),] 
asat <- subset(asat, select = c("PlotRepeat", "Plot","Repeat", "Rep", "Name", "Photo", "gs"))
names(asat)[6] <- "asat"
asat$iWUE <- asat$asat/asat$gs #calculate water use efficiency

output <- vector()

for (i in 1:length(fitting)) {
  x <- fitting[[i]]
  x <- x$Photosyn(Ci=400)$ALEAF
  output <- append(output, x)
}

asat$a400 <- output
asat$sl <- (asat$a400 - asat$asat)/asat$a400 #calculate 
asat <- asat[,-1]
rownames(asat) <- NULL

ggplot(asat, aes(x=reorder(Name, iWUE, FUN = median), y=iWUE)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Genotype") + ylab("iWUE")

ggplot(asat, aes(x=reorder(Name, sl, FUN = median), y=sl)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Genotype") + ylab("Stomatal Limitation")

ggplot(asat, aes(x=sl, y=iWUE)) + geom_point()

#write these as a file so i can calculate blups! 

write_xlsx(asat, "~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/extra_aci_parms.xlsx")
