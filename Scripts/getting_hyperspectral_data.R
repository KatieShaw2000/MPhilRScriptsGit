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
# plot(as.numeric(get_spectra), type='l')

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

tidied_data <- tidied_data[!(tidied_data$Repeat_two=="0"),]

tidied_data <- tidied_data[,-c(5:154)]

#plot the raw data ----

# tidied_data <- tidied_data[!(tidied_data$Repeat_two=="0"),]
# 
# tidied_data$PlotRepeatRepeat2 <- paste(tidied_data$Plot, tidied_data$Repeat, tidied_data$Repeat_two)
# 
# long_hyperspectral <- tidied_data %>%
#   gather(Wavelength, Data, -Date, -Plot, -Repeat, -Repeat_two, -PlotRepeatRepeat2)
# 
# PDFpath1_1 <- "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/Hyperspectral files/AllPlots.pdf"
# pdf(file=PDFpath1_1)
# 
# for (value in unique(long_hyperspectral$PlotRepeatRepeat2)){
#   subset <- subset(long_hyperspectral, long_hyperspectral$PlotRepeatRepeat2 == value)
#   plot(subset$Wavelength, subset$Data, col='Black', xlim=c(500,2500), ylim=c(0,0.8),
#        main=paste("Plot of", value,"_ 1"), xlab="Wavelength")
# }
# 
# dev.off()

#get means ----

hyperspectral_means <- aggregate(tidied_data[,5:2005], list(tidied_data$Plot, tidied_data$Repeat), mean)
names(hyperspectral_means)[1] <- "Plot"
names(hyperspectral_means)[2] <- "Repeat"
hyperspectral_means <- hyperspectral_means[-c(1487:1489),]

#make this the file! 
write.csv(hyperspectral_means,"~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/hyperspectral.csv")

#get the files into format to use the wheat predictor ----

for_predictor <- hyperspectral_means
for_predictor$PlotRepeat <- paste(for_predictor$Plot, for_predictor$Repeat)
for_predictor <- for_predictor %>% 
  select(c(PlotRepeat), everything())
for_predictor <- for_predictor[,-c(2,3)]
for_predictor <- t(for_predictor)
for_predictor <- as.data.frame(for_predictor)

names <- for_predictor[1,] #making first row the column names 
colnames(for_predictor) <- names
for_predictor <- for_predictor[-1,]

rows <- rownames(for_predictor) #making the rownames the first column
rownames(for_predictor) <- NULL
for_predictor <- cbind(rows, for_predictor)
names(for_predictor)[1] <- "Wavelength"

#get csvs for every 200 columns ----

spec_1 <- for_predictor[,c(1:201)]
write.csv(spec_1,"~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/hyperspec wheat predictor/spec_1.csv",row.names=F)

spec_2 <- for_predictor[,c(1,202:401)]
write.csv(spec_2,"~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/hyperspec wheat predictor/spec_2.csv",row.names=F)

spec_3 <- for_predictor[,c(1,402:601)]
write.csv(spec_3,"~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/hyperspec wheat predictor/spec_3.csv",row.names=F)

spec_4 <- for_predictor[,c(1,602:801)]
write.csv(spec_4,"~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/hyperspec wheat predictor/spec_4.csv",row.names=F)

spec_5 <- for_predictor[,c(1,802:1001)]
write.csv(spec_5,"~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/hyperspec wheat predictor/spec_5.csv",row.names=F)

spec_6 <- for_predictor[,c(1,1002:1201)]
write.csv(spec_6,"~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/hyperspec wheat predictor/spec_6.csv",row.names=F)

spec_7 <- for_predictor[,c(1,1202:1401)]
write.csv(spec_7,"~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/hyperspec wheat predictor/spec_7.csv",row.names=F)

spec_8 <- for_predictor[,c(1,1402:1486)]
write.csv(spec_8,"~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/hyperspec wheat predictor/spec_8.csv",row.names=F)
