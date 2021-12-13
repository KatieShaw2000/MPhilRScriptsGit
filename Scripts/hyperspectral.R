#packages 

library(asdreader)

#set working directory 

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/Hyperspectral files")

#try and read the data in!

paths_to_spectra <- list.files(pattern='*.asd', recursive = TRUE)
get_spectra <- get_spectra(paths_to_spectra)
plot(as.numeric(get_spectra), type='l')
