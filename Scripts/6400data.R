#packages
library(readxl)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(ggforce)
library(data.table)

#Set working directory ---- 

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/Cleaned 6400 files")
file.list <- list.files(pattern='*.xlsx', recursive = TRUE)
LICOR_6400.list <- lapply(file.list, read_excel)
ACi_6400 <- rbindlist(LICOR_6400.list, fill = TRUE)

#visualise ----

without_drop <- filter(ACi_6400, Light == "1800")

#A without light drop 

ggplot(data = without_drop, aes(x = Elapsed_time, y = Photo, colour = Repeat)) +
  xlab("Elapsed Time") + 
  ylab("A")+
  ggtitle("A without light drop") +
  ylim(0,50) +
  xlim(0,500)+
  geom_point(size=0.5)+
  theme(legend.position = "none") + 
  facet_wrap_paginate( ~ Plot, ncol= 8, nrow = 5, page = 1)

#Gs without light drop 

ggplot(data = without_drop, aes(x = Elapsed_time, y = Cond, colour = Repeat)) +
  xlab("Elapsed Time") + 
  ylab("gs")+
  ggtitle("Gs without light drop")+
  ylim(0,1) +
  xlim(0,500)+
  geom_point(size=0.5)+
  theme(legend.position = "none") + 
  facet_wrap_paginate( ~ Plot, ncol= 8, nrow = 5, page = 1)

#A with light drop

ggplot(data = ACi_6400, aes(x = Elapsed_time, y = Photo, colour = Repeat)) +
  xlab("Elapsed Time") + 
  ylab("A")+
  ggtitle("A with light drop")+
  ylim(0, 50) +
  geom_point(size=0.5)+
  theme(legend.position = "none") + 
  facet_wrap_paginate( ~ Plot, ncol= 8, nrow = 5, page = 1)


#Gs with light drop 

ggplot(data = ACi_6400, aes(x = Elapsed_time, y = Cond, colour = Repeat)) +
  xlab("Elapsed Time") + 
  ylab("gs")+
  ggtitle("Gs with light drop")+
  ylim(0, 1) +
  geom_point(size=0.5)+
  theme(legend.position = "none") + 
  facet_wrap_paginate( ~ Plot, ncol= 8, nrow = 5, page = 1)
