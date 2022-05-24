#get libraries ----

library(readr)
library(dplyr)
library(tidyr)

#set working directory ---- 

setwd("~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/hyperspec wheat predictor")

#get the original data as one data frame (this is just the hyperspectral csv file) ---- 

original <- read_csv("~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/hyperspectral.csv")
original <- original[,-1]
original$PlotRepeat <- paste(original$Plot, original$Repeat)
original <- original %>% select(PlotRepeat, everything())
original <- original[,-c(2,3)]

#get the corrected data as one data frame ----

jump_1 <- read_csv("spec_1_Corrected_Spectra_after_jump.csv",col_names=FALSE)
jump_2 <- read_csv("spec_2_Corrected_Spectra_after_jump.csv",col_names=FALSE)
jump_3 <- read_csv("spec_3_Corrected_Spectra_after_jump.csv",col_names=FALSE)
jump_4 <- read_csv("spec_4_Corrected_Spectra_after_jump.csv",col_names=FALSE)
jump_5 <- read_csv("spec_5_Corrected_Spectra_after_jump.csv",col_names=FALSE)
jump_6 <- read_csv("spec_6_Corrected_Spectra_after_jump.csv",col_names=FALSE)
jump_7 <- read_csv("spec_7_Corrected_Spectra_after_jump.csv",col_names=FALSE)
jump_8 <- read_csv("spec_8_Corrected_Spectra_after_jump.csv",col_names=FALSE)

names <- c("PlotRepeat", 500:2500)

colnames(jump_1) <- names
colnames(jump_2) <- names
colnames(jump_3) <- names
colnames(jump_4) <- names
colnames(jump_5) <- names
colnames(jump_6) <- names
colnames(jump_7) <- names
colnames(jump_8) <- names

corrected <- rbind(jump_1,jump_2,jump_3,jump_4,jump_5,jump_6,jump_7,jump_8)

rm(jump_1,jump_2,jump_3,jump_4,jump_5,jump_6,jump_7,jump_8,names)

#plot the original spectra ----

wavelength <- seq(500,2500,1)

PDFpath1 <- "~/OneDrive - University of Cambridge/MPhil/Original_spectra.pdf"
pdf(file=PDFpath1)

for (i in 1:nrow(original)){
  plot(wavelength, original[i,2:2002], col='Black', xlim=c(500,2500), ylim=c(0,0.8),
       main=paste("Plot of", original[i,1]), xlab="Wavelength", ylab = "Reflectance")
}

dev.off()

#plot the website jump corrected spectra ----

PDFpath2 <- "~/OneDrive - University of Cambridge/MPhil/Corrected_spectra.pdf"
pdf(file=PDFpath2)

for (i in 1:nrow(corrected)){
  plot(wavelength, corrected[i,2:2002], col='Black', xlim=c(500,2500), ylim=c(0,0.8),
       main=paste("Plot of", original[i,1]), xlab="Wavelength", ylab = "Reflectance")
}

dev.off()

write.csv(corrected,"~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/hyperspectral_corrected.csv",row.names=F)


#putting parameter PLSR data frame from the wheat predictor into one data frame ----

setwd("~/Desktop")
jump_1 <- read_csv("spec_1_PLSR.csv",col_names=FALSE)
jump_2 <- read_csv("spec_2_PLSR.csv",col_names=FALSE)
jump_3 <- read_csv("spec_3_PLSR.csv",col_names=FALSE)
jump_4 <- read_csv("spec_4_PLSR.csv",col_names=FALSE)
jump_5 <- read_csv("spec_5_PLSR.csv",col_names=FALSE)
jump_6 <- read_csv("spec_6_PLSR.csv",col_names=FALSE)
jump_7 <- read_csv("spec_7_PLSR.csv",col_names=FALSE)
jump_8 <- read_csv("spec_8_PLSR.csv",col_names=FALSE)

parms <- rbind(jump_1,jump_2,jump_3,jump_4,jump_5,jump_6,jump_7,jump_8)

write.csv(parms,"~/Desktop/wheat_predictor_parms.csv",row.names=F)
