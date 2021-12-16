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

#make this the file! 
write.csv(tidied_data,"~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/hyperspectral.csv")
