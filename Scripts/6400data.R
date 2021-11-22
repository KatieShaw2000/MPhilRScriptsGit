#packages
library(readxl)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(ggforce)

#Set working directory ---- 

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/Cleaned 6400 files")
file.list <- list.files(pattern='*.xlsx', recursive = TRUE)
LICOR_6400.list <- lapply(file.list, read_excel)
ACi_6400 <- rbindlist(LICOR_6400.list, fill = TRUE)

#visualise

ggplot(data = ACi_6400, aes(x = Elapsed_time, y = Photo, colour = Repeat)) +
  xlab("Elapsed Time") + 
  ylab("Photo")+
  ylim(0, 50) +
  geom_point(size=0.1)+
  theme(legend.position = "none") + 
  facet_wrap_paginate( ~ Plot, ncol= 8, nrow = 5, page = 1)

ggplot(data = ACi_6400, aes(x = Elapsed_time, y = Cond, colour = Repeat)) +
  xlab("Elapsed Time") + 
  ylab("Cond")+
  ylim(0, 1) +
  geom_point(size=0.1)+
  theme(legend.position = "none") + 
  facet_wrap_paginate( ~ Plot, ncol= 8, nrow = 5, page = 1)
