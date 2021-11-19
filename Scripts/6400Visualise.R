setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign")

#packages
library(readxl)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(ggforce)

#get data
CAP1_jun1 <- read_excel("Cleaned 6400 files/June 1/CAP1_barley_June1.xlsx")
CAP2_jun1 <- read_excel("Cleaned 6400 files/June 1/CAP2_barley_June1.xlsx")
CAP3_jun1 <- read_excel("Cleaned 6400 files/June 1/CAP3_barley_June1.xlsx")
CAP4_jun1 <- read_excel("Cleaned 6400 files/June 1/CAP4_barley_June1.xlsx")
CAP5_jun1 <- read_excel("Cleaned 6400 files/June 1/CAP5_barley_June1.xlsx")
CAP6_jun1 <- read_excel("Cleaned 6400 files/June 1/CAP6_barley_June1.xlsx")

jun1 <- rbind(CAP1_jun1, CAP2_jun1, CAP3_jun1, CAP4_jun1, CAP5_jun1, CAP6_jun1)

CAP1_jun2 <- read_excel("Cleaned 6400 files/June 2/CAP1_barley_June2.xlsx")
CAP2_jun2 <- read_excel("Cleaned 6400 files/June 2/CAP2_barley_June2.xlsx")
CAP3_jun2 <- read_excel("Cleaned 6400 files/June 2/CAP3_barley_June2.xlsx")
CAP4_jun2 <- read_excel("Cleaned 6400 files/June 2/CAP4_barley_June2.xlsx")
CAP5_jun2 <- read_excel("Cleaned 6400 files/June 2/CAP5_barley_June2.xlsx")
CAP6_jun2 <- read_excel("Cleaned 6400 files/June 2/CAP^_barley_June2.xlsx")

jun2 <- rbind(CAP1_jun2, CAP2_jun2, CAP3_jun2, CAP4_jun2, CAP5_jun2, CAP6_jun2)

#visualise

jun1_graphs <- ggplot(data = jun1, aes(x=Elapsed_time, y = Photo, colour = Repeat)) +
  ylim(0,35)+
  xlab("") + 
  geom_point(size=1)+
  theme(legend.position = "none") + 
  facet_wrap_paginate( ~ Plot, ncol= 7, nrow = 5, page =1)

jun2_graphs <- ggplot(data = jun2, aes(x=Elapsed_time, y = Photo, colour = Repeat)) +
  ylim(0,35)+
  xlab("") + 
  geom_point(size=1)+
  theme(legend.position = "none") + 
  facet_wrap_paginate( ~ Plot, ncol= 7, nrow = 5, page =2)


