#set working directory 

setwd("~/OneDrive - University of Cambridge/MPhil/GitLink")

#Get packages ----

library(readxl)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(ggforce)
library(tidyverse)
library(dplyr)
library(writexl)
library(segmented)
library(minpack.lm)
library(data.table)

#get data ----

NPQdata <- read_excel("Final Scripts Output/NPQ_initial_dataset.xlsx")
NPQdata$plot_id <- paste(NPQdata$Plot, NPQdata$Repeat)

#make function to use on all data for induction NPQ----

induction_data <- filter(NPQdata, cumulative_time <= 600)

NPQ_induction_fitting <- function(dataset,plot,a=3.5, b=0.005){
  
  subset <- subset(dataset, plot_id == plot) #subsets for each unique plot/repeat
  induction_model <- nlsLM(NPQ_values ~ a*(1-exp(-b*cumulative_time)),data=subset, 
                           start = list(a=a, b=b))
  a_fit <- induction_model$m$getAllPars()[1]
  a_confint <- confint(induction_model)[1,2] - confint(induction_model)[1,1]
  b_fit <- induction_model$m$getAllPars()[2]
  b_confint <- confint(induction_model)[2,2] - confint(induction_model)[2,1]
  induction_fitting <- data.frame(plot = plot, 
                                 a_fit = a_fit,
                                 a_confint = a_confint,
                                 b_fit = b_fit,
                                 b_confint=b_confint)
  
}

induction_plot_ids <-as.character(unique(induction_data$plot_id))
names(induction_plot_ids) <- induction_plot_ids

all_induction_fits <- lapply(induction_plot_ids, function(x) NPQ_induction_fitting(induction_data,x))
all_induction_fits <- rbindlist(all_induction_fits)

#make function to use on all data for relaxation NPQ----

relaxation_data <- filter(NPQdata, cumulative_time >= 600)

NPQ_relaxation_fitting <- function(dataset,plot,c=3,d=0.005,e=0.5){
  
  subset <- subset(dataset, plot_id == plot) #subsets for each unique plot/repeat
  relaxation_model <- nlsLM(NPQ_values ~ c*(exp(-d*time_post_light_off)) + e, data=subset,
                            start=list(c=c,d=d,e=e))
  c_fit <- relaxation_model$m$getAllPars()[1]
  d_fit <- relaxation_model$m$getAllPars()[2]
  e_fit <- relaxation_model$m$getAllPars()[3]
  relaxation_fitting <- data.frame(plot = plot, 
                                 c_fit = c_fit,
                                 d_fit = d_fit,
                                 e_fit=e_fit)
  
}

relaxation_plot_ids <-as.character(unique(relaxation_data$plot_id))
names(relaxation_plot_ids) <- relaxation_plot_ids

all_relaxation_fits <- lapply(relaxation_plot_ids, function(x) NPQ_relaxation_fitting(relaxation_data,x))
all_relaxation_fits <- rbindlist(all_relaxation_fits)

#initial slope NPQ----

initial_data <- filter(NPQdata, cumulative_time <= 40)
initial_data <- select(initial_data, plot_id,Plot,Repeat, NPQ_values, cumulative_time)

initial_slope_model <- function(dataset, plot) {
  
  subset <- subset(dataset, plot_id==plot)
  initial_model <- lm(NPQ_values ~ 0 + cumulative_time, data=subset) #the 0 tells it to fit through the origin
  initial_slope_fitting <- data.frame(plot=plot,
                                      gradient=coef(initial_model))
}

initial_slope_plot_ids <-as.character(unique(initial_data$plot_id))
names(initial_slope_plot_ids) <- initial_slope_plot_ids

initial_slope_fits <- lapply(initial_slope_plot_ids, function(x) initial_slope_model(initial_data,x))
initial_slope_fits <- rbindlist(initial_slope_fits)

initial_data <- filter(NPQdata, cumulative_time == 40) #do this for later to use in final dataset of parms
initial_data <- select(initial_data, plot_id, Plot, Repeat)

#maximum amplitude NPQ----

max_amp <- filter(NPQdata, NPQ_names=="NPQ_Lss")
max_amp <- select(max_amp, plot_id, NPQ_values)
names(max_amp)[2] <- "max_amp"

#end point NPQ ----

end_point <- filter(NPQdata, NPQ_names == "NPQ_D8")
end_point <- select(end_point, plot_id, NPQ_values, 'Fv/Fm_values')
names(end_point)[2] <- "end_NPQ"
names(end_point)[3] <- "end_FvFm"

# phi PSII in the dark parameters----

names(NPQdata)[5] <- "FvFm_values"
relaxation_data <- filter(NPQdata, cumulative_time >= 600)

#uses relaxation_data ----

PSII_dark_fitting <- function(dataset,plot,f=0.25,g=0.002,h=0.6){
  
  subset <- subset(dataset, plot_id == plot) #subsets for each unique plot/repeat
  PSII_model <- nlsLM(FvFm_values ~ f*(1-exp(-g*time_post_light_off)) + h,data=subset, 
                           start = list(f=f,g=g,h=h))
  f_fit <- PSII_model$m$getAllPars()[1]
  g_fit <- PSII_model$m$getAllPars()[2]
  h_fit <- PSII_model$m$getAllPars()[3]

  PSII_fitting <- data.frame(plot = plot,
                             f_fit = f_fit,
                             g_fit = g_fit,
                             h_fit = h_fit)
  
}

PSII_plot_ids <-as.character(unique(relaxation_data$plot_id))
names(PSII_plot_ids) <- PSII_plot_ids

all_PSII_fits <- lapply(PSII_plot_ids, function(x) PSII_dark_fitting(relaxation_data,x))
all_PSII_fits <- rbindlist(all_PSII_fits)

#filter based on confint for some induction parameters ----

all_induction_fits <- filter(all_induction_fits, a_confint <=2)
all_induction_fits <- filter(all_induction_fits, b_confint <= 0.01)

#get all parameters into one dataframe ----

NPQ_parms <- merge(all_induction_fits, all_relaxation_fits,by="plot")
NPQ_parms <- merge(NPQ_parms, all_PSII_fits, by = "plot")
NPQ_parms <- merge(NPQ_parms, initial_slope_fits, by = "plot")
names(NPQ_parms)[1] <- "plot_id"
NPQ_parms <- merge(NPQ_parms, max_amp, by="plot_id")
NPQ_parms <- merge(NPQ_parms, end_point, by="plot_id")

#tidy data based on confint for induction parameters? ----

# ggplot(NPQ_parms, aes(x=plot_id, y=a_confint)) + geom_point() 
# ggplot(NPQ_parms, aes(x=factor(0), y=a_confint)) + geom_boxplot()
# ggplot(NPQ_parms, aes(x=a_confint)) + geom_density() #geom_vline(aes(xintercept=2)) #suggest the cut off is 2 for a! 
# 
# ggplot(NPQ_parms, aes(x=plot_id, y=b_confint)) + geom_point() 
# ggplot(NPQ_parms, aes(x=factor(0), y=b_confint)) + geom_boxplot() # suggest cut off is 0.01 for b!
# ggplot(NPQ_parms, aes(x=b_confint)) + geom_density() #geom_vline(aes(xintercept=0.01)) #suggest the cut off is 0.01 for b!

#final dataset ----

NPQ_parms <- merge(initial_data, NPQ_parms, by="plot_id")
NPQ_parms <- select(NPQ_parms, -c(a_confint, b_confint, plot_id))

#add in genotype to dataframe ----

genotype_data <- read_excel("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/FieldDesign.xlsx")
names(genotype_data)[1] <- "Plot"

NPQ_parms <- merge(NPQ_parms, genotype_data, by = "Plot")

NPQ_parms <- NPQ_parms[,c(19,18,1,2,15,16,3:14)]

write_xlsx(NPQ_parms, "~/OneDrive - University of Cambridge/MPhil/GitLink/Final Scripts Output/NPQ_parms.xlsx")
