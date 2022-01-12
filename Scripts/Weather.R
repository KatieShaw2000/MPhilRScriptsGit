#packages
library(readxl)
library(ggplot2)
library(tidyr)
library(ggforce)
library(tidyverse)

#Set working directory 

setwd("~/OneDrive - University of Cambridge/MPhil")

#get data

hubner_weather_data <- read_excel("~/OneDrive - University of Cambridge/MPhil/hubner weather data.xlsx")
GenotypeSite <- read_excel("~/OneDrive - University of Cambridge/MPhil/GenotypeSite.xlsx")

weather_data <- merge(GenotypeSite, hubner_weather_data, by = "Site")

write_csv(weather_data, "~/OneDrive - University of Cambridge/MPhil/genotype_weather.csv")
