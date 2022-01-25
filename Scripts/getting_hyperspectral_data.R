#packages 

library(asdreader)
library(pls)
library(dplyr)
library(reshape2)
library(here)
library(plotrix)
library(ggplot2)
library(gridExtra)
library(remotes)
library(devtools)
library(readr)
library(RCurl)
library(httr)
library(scales)
library(knitr)
library(tidyr)

#set working directory ----

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/Hyperspectral files")

#read data ----

paths_to_spectra <- list.files(pattern='*.asd', recursive = TRUE)
get_spectra <- get_spectra(paths_to_spectra)
plot(as.numeric(get_spectra), type='l')

spectra <- as.data.frame(get_spectra)
spectra$info <- rownames(spectra) #get names as a column
spectra <- spectra %>%
  select(info, everything()) #move names columns to the start 
row.names(spectra) <- NULL

test <- separate(data=spectra, col=info, sep = " ", into = c("Date", "B", "leafID"))
test$Date <- paste(test$Date, test$B)
test <- test[,-2]
test$leafID <- gsub("2021/", "", as.character(test$leafID))
test$leafID <- gsub(".asd", "", as.character(test$leafID))
test$leafID <- gsub("0000", "", as.character(test$leafID))
test$leafID <- gsub("Hs-Plot", "", as.character(test$leafID))
test$leafID <- gsub("-Rep", "", as.character(test$leafID))
test$leafID <- gsub("000", "", as.character(test$leafID))
test$leafID <- gsub("_", "", as.character(test$leafID))
test$leafID <- gsub("-", "", as.character(test$leafID))

test$Plot <- substr(test$leafID, 0, 4)
test$Repeat <- substr(test$leafID, 5,5)
test$Repeat_two <- substr(test$leafID, 6,6)

tidied_data <- test %>%
  select(c(Date, Plot, Repeat, Repeat_two), everything())

tidied_data <- tidied_data[,-5]

#plot the raw data ----

# hyperspectral <- hyperspectral[!(hyperspectral$Repeat_two == "0"),] #remove the ones where repeat_2 is 0
# 
# long_hyperspectral <- hyperspectral %>%
#   gather(Wavelength, Data, -Date, -Plot, -Repeat, -Repeat_two)
# 
# PDFpath1_1 <- "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/Hyperspectral files/Repeat1_raw.pdf"
# pdf(file=PDFpath1_1)
# 
# Repeat1 <- subset(long_hyperspectral, long_hyperspectral$Repeat == 1)
# 
# for (value in unique(Repeat1$Plot)){
#   subset <- subset(Repeat1, Repeat1$Plot == value)
#   plot(subset$Wavelength, subset$Data, col='Black', xlim=c(350,2500), ylim=c(0,0.8), 
#        main=paste("Plot of", value,"_ 1"), xlab="Wavelength")
# }
# 
# dev.off()
# 
# PDFpath2_1 <- "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/Hyperspectral files/Repeat2_raw.pdf"
# pdf(file=PDFpath2_1)
# 
# Repeat2 <- subset(long_hyperspectral, long_hyperspectral$Repeat == 2)
# 
# for (value in unique(Repeat2$Plot)){
#   subset <- subset(Repeat2, Repeat2$Plot == value)
#   plot(subset$Wavelength, subset$Data, col='Black', xlim=c(350,2500), ylim=c(0,0.8), 
#        main=paste("Plot of", value,"_ 2"), xlab="Wavelength")
# }
# 
# dev.off()
# 
# PDFpath3_1 <- "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/Hyperspectral files/Repeat3_raw.pdf"
# pdf(file=PDFpath3_1)
# 
# Repeat3 <- subset(long_hyperspectral, long_hyperspectral$Repeat == 3)
# 
# for (value in unique(Repeat3$Plot)){
#   subset <- subset(Repeat3, Repeat3$Plot == value)
#   plot(subset$Wavelength, subset$Data, col='Black', xlim=c(350,2500), ylim=c(0,0.8), 
#        main=paste("Plot of", value,"_ 3"), xlab="Wavelength")
# }
# 
# dev.off()

tidied_data <- tidied_data[!(tidied_data$Repeat_two=="0"),]

#get means ----

hyperspectral_means <- aggregate(tidied_data[, 5:2155], list(tidied_data$Date, tidied_data$Plot, tidied_data$Repeat), mean)
names(hyperspectral_means)[1] <- "Date"
names(hyperspectral_means)[2] <- "Plot"
names(hyperspectral_means)[3] <- "Repeat"

#make this the file! 
write.csv(hyperspectral_means,"~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/hyperspectral.csv")
