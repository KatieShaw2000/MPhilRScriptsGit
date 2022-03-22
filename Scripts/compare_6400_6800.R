#libraries ----

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

#get data ----

data_6800 <- read.csv("reduced_df.csv")
data_6800 <- select(data_6800, Name, Plot, Repeat, Photo)

data_6400 <- read_xlsx("~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/6400data.xlsx")
data_6400 <- select(data_6400, Name, Plot, Repeat, mean_Asat)

#get max ACi points ----

max_6800 <- ddply(data_6800, .(Plot, Repeat), summarise, max_6800 = max(Photo, na.rm = TRUE)) 

#combine and visualise ----

combined <- merge(max_6800, data_6400, by = c("Plot", "Repeat"))

plot(combined$max_6800, combined$mean_Asat, xlab = "6800 Amax", ylab = "6400 Asat", 
     xlim = c(0,60), ylim = c(0,60))

cor.test(combined$max_6800, combined$mean_Asat, method = c("pearson"))

ggplot(combined, aes(x=max_6800, y=mean_Asat)) + geom_point() + geom_smooth(method='lm') +
  xlim(0,60) + ylim(0,60) +
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title = element_text(size=20))
