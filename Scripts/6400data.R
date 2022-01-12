#packages
library(readxl)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(ggforce)
library(data.table)
library(forcats)
library(plyr)
library(lme4)
library(inti)
library(writexl)
library(tidyverse)

#Set working directory 

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/Cleaned 6400 files")
file.list <- list.files(pattern='*.xlsx', recursive = TRUE)
LICOR_6400.list <- lapply(file.list, read_excel)
ACi_6400 <- rbindlist(LICOR_6400.list, fill = TRUE)

ACi_6400$Repeat <- as.factor(ACi_6400$Repeat)

#visualise ----

without_drop <- filter(ACi_6400, Light == "1800")

#remove plots based on graphs that look like experiment was wrong ----

#think the easiest way is to merge plotrepeat and then remove ones based on this column 

ACi_6400$PlotRepeat <- paste(ACi_6400$Plot, ACi_6400$Repeat)

to_remove <- c("1012 2", "1026 2", "1062 1", "1129 1", "1148 2", "1167 2", "1173 1", "1199 2",
               "1216 1", "1222 2", "1254 1", "1263 1", "1318 3", "2015 3", "2028 1", "2031 3", 
               "2036 3", "2045 1", "2051 3", "2082 3", "2100 2", "2103 3", "2122 2", "2135 1",
               "2164 2", "2169 1", "2191 2", "2333 3", "2241 3", "2242 2", "2275 3")

ACi_6400 <- ACi_6400[!ACi_6400$PlotRepeat %in% to_remove,]

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
  facet_wrap_paginate( ~ Plot, ncol= 5, nrow = 5, page = 1)

#A with light drop

ggplot(data = ACi_6400, aes(x = Elapsed_time, y = Photo, colour = Repeat)) +
  xlab("Elapsed Time") + 
  ylab("A")+
  ggtitle("A with light drop")+
  ylim(0, 50) +
  geom_point(size=0.5)+
  facet_wrap_paginate( ~ Plot, ncol= 5, nrow = 5, page = 1)


#Gs with light drop 

ggplot(data = ACi_6400, aes(x = Elapsed_time, y = Cond, colour = Repeat)) +
  xlab("Elapsed Time") + 
  ylab("gs")+
  ggtitle("Gs with light drop")+
  ylim(0, 1) +
  geom_point(size=0.5)+
  facet_wrap_paginate( ~ Plot, ncol= 8, nrow = 5, page = 16)

#remove plots based on graphs that look like experiment was wrong ----

#think the easiest way is to merge plotrepeat and then remove ones based on this column 

# ACi_6400$PlotRepeat <- paste(ACi_6400$Plot, ACi_6400$Repeat)
# 
# to_remove <- c("1012 2", "1026 2", "1062 1", "1129 1", "1148 2", "1167 2", "1173 1", "1199 2",
#                "1216 1", "1222 2", "1254 1", "1263 1", "1318 3", "2015 3", "2028 1", "2031 3", 
#                "2036 3", "2045 1", "2051 3", "2082 3", "2100 2", "2103 3", "2122 2", "2135 1",
#                "2164 2", "2169 1", "2191 2", "2333 3", "2241 3", "2242 2", "2275 3")
# 
# ACi_6400 <- ACi_6400[!ACi_6400$PlotRepeat %in% to_remove,]

# get averages of last 10 points before end of light phase ----

before_drop <- filter(ACi_6400, Light == "1800")
after_drop <- filter(ACi_6400, Light == "200")

before_drop <- filter(before_drop, Elapsed_time >= 372) #get last minute
after_drop <- filter(after_drop, Elapsed_time >= 1020) #get last minute
                       
mean_Asat <- ddply(before_drop, .(Plot, Repeat), summarise, mean_Asat = mean(Photo, na.rm = TRUE), 
                   sd_Asat = sd(Photo, na.rm=TRUE), se_Asat = sd_Asat/sqrt(10)) 

mean_gs_Asat <- ddply(before_drop, .(Plot, Repeat), summarise, mean_gs_Asat = mean(Cond, na.rm = TRUE),
                      sd_gs_Asat = sd(Cond, na.rm=TRUE), se_gs_Asat = sd_gs_Asat/sqrt(10)) 
 
mean_Alow <- ddply(after_drop, .(Plot, Repeat), summarise, mean_Alow = mean(Photo, na.rm = TRUE), 
                   sd_Alow = sd(Photo, na.rm=TRUE), se_Alow = sd_Alow/sqrt(10)) 

mean_gs_Alow <- ddply(after_drop, .(Plot, Repeat), summarise, mean_gs_Alow = mean(Cond, na.rm = TRUE),
                      sd_gs_Alow = sd(Cond, na.rm=TRUE), se_gs_Alow = sd_gs_Alow/sqrt(10)) 

ggplot(data=mean_Asat, aes(x=Plot, y=mean_Asat)) + geom_point()
ggplot(data=mean_gs_Asat, aes(x=Plot, y=mean_gs_Asat)) + geom_point()
ggplot(data=mean_Alow, aes(x=Plot, y=mean_Alow)) + geom_point()
ggplot(data=mean_gs_Alow, aes(x=Plot, y=mean_gs_Alow)) + geom_point()

ggplot(data=mean_Asat, aes(x=reorder(Plot, mean_Asat, FUN = median), y = mean_Asat)) + geom_boxplot()

# ggplot(data=mean_Asat, aes(x=se_Asat)) + geom_density() + geom_vline(aes(xintercept=0.4)) #suggest 0.4 cutoff 
# ggplot(data=mean_Alow, aes(x=se_Alow)) + geom_density() + xlim(0,3) #suggest 0.4 cutoff again
# 
# ggplot(data=mean_gs_Asat, aes(x=se_gs_Asat)) + geom_density() #suggest 0.005 cutoff
# ggplot(data=mean_gs_Alow, aes(x=se_gs_Alow)) + geom_density() +xlim(0,0.005) #suggest 0.005 cutoff

#make a big dataframe and then cut it down based on standard errors ----

cleaned_6400_data <- merge(mean_Alow, mean_Asat, by = c("Plot", "Repeat"))
cleaned_6400_data <- merge(cleaned_6400_data, mean_gs_Alow, by = c("Plot", "Repeat"))
cleaned_6400_data <- merge(cleaned_6400_data, mean_gs_Asat, by = c("Plot", "Repeat"))

cleaned_6400_data <- filter(cleaned_6400_data, se_Asat <= 0.4, se_Alow <= 0.4, se_gs_Asat <= 0.005, se_gs_Alow <= 0.005)

cleaned_6400_data <- select(cleaned_6400_data, "Plot", "Repeat", "mean_Asat", "mean_Alow", "mean_gs_Asat", "mean_gs_Alow")

#get genotype and other factors in ----

fielddesign <- read_xlsx("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/FieldDesign.xlsx")
names(fielddesign)[1] <- "Plot"

other_info <- select(ACi_6400, "Date", "Start_Hour", "CAP", "Plot", "Repeat")
other_info <- unique(other_info) #remove duplicates

cleaned_6400_data <- merge(fielddesign, cleaned_6400_data, by = "Plot")
cleaned_6400_data <- merge(cleaned_6400_data, other_info, by = c("Plot", "Repeat"))

data <- merge(fielddesign, mean_Asat, by = "Plot")
ggplot(data=data, aes(x=reorder(Name, mean_Asat, FUN = median), y = mean_Asat)) + geom_boxplot() +
  xlab("Genotype")

#export this so i can use in another script 
write_xlsx(cleaned_6400_data,"~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/6400data.xlsx")
