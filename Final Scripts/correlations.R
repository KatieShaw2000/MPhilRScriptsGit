#set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/GitLink/Final Scripts Output")

#get packages ----

library(ggplot2)
library(readxl)
library(dplyr)
library(corrplot)
library(ggpubr)
library(Hmisc)

#get data ---- 

all_blues <- read.csv("all_blues.csv")
all_blues <- all_blues[,-1]

#testing for normality of NPQ parameters ----

NPQ <- all_blues[c(1,14:25)]
NPQ <- na.omit(NPQ)

hist(NPQ$a_fit)
shapiro.test(NPQ$a_fit) #normally distributed 

shapiro.test(NPQ$b_fit) #not normally distributed
ggplot(data=NPQ, aes(x=factor(0), y=b_fit)) + geom_boxplot()
hist(NPQ$b_fit)

log_b_fit <- log10(NPQ$b_fit)
hist(log_b_fit)
shapiro.test(log_b_fit) #now normally distributed 

NPQ <- NPQ[,-c(3,7,8,9,13)]
NPQ$log_b_fit <- log_b_fit

hist(NPQ$c_fit) 
shapiro.test(NPQ$c_fit) #normally distributed

shapiro.test(NPQ$d_fit) #not normally distributed 
hist(NPQ$d_fit)
ggplot(data=NPQ, aes(x=factor(0), y=d_fit)) + geom_boxplot()

log_d_fit <- log10(NPQ$d_fit)
hist(log_d_fit)
shapiro.test(log_d_fit) #now normally distributed

NPQ <- NPQ[,-4]
NPQ$log_d_fit <- log_d_fit

hist(NPQ$e_fit)
shapiro.test(NPQ$e_fit) #not normally distributed

log_e_fit <- log10(NPQ$e_fit)
hist(log_e_fit)
shapiro.test(log_e_fit) #now normally distributed

NPQ <- NPQ[,-4]
NPQ$log_e_fit <- log_e_fit

hist(NPQ$end_NPQ)
shapiro.test(NPQ$end_NPQ) #not normally distributed

log_end_NPQ <- log10(NPQ$end_NPQ)
hist(log_end_NPQ)
shapiro.test(log_end_NPQ) #now normally distributed 

NPQ <- NPQ[,-4]
NPQ$log_end_NPQ <- log_end_NPQ

hist(NPQ$max_amp)
shapiro.test(NPQ$max_amp) #normally distributed

hist(NPQ$gradient)
shapiro.test(NPQ$gradient) #normally distributed 

NPQ <- NPQ[,c(1,2,6,3,7,8,4,5,9)]

#NPQ correlation plot ----

names(NPQ)[2] <- "a_induction"
names(NPQ)[3] <- "log10(b_induction)"
names(NPQ)[4] <- "a_relaxation"
names(NPQ)[5] <- "log10(b_relaxation)"
names(NPQ)[6] <- "log10(c_relaxation)"
names(NPQ)[9] <- "log10(end_NPQ)" 

NPQ <- NPQ[,-8]

corrplot(cor(NPQ[,c(2:8)]), type = "upper", diag = FALSE, method = "circle", title = "NPQ Parameter Correlations", mar=c(0,0,1,0))

rcorr(as.matrix(NPQ[,2:8]))

cor.test(NPQ$a_induction, NPQ$`log10(b_induction)`, method = "pearson")

#ACi parameters: test for normality ----

aci_accessions <- all_blues[,1:13]

aci_accessions <- na.omit(aci_accessions)

hist(aci_accessions$days_to_heading)
shapiro.test(aci_accessions$days_to_heading) #not normally distributed --> log transformation doesn't make it normal so leave out of plot?

hist(aci_accessions$SLA)
shapiro.test(aci_accessions$SLA) #normally distributed 

hist(aci_accessions$mean_Asat)
shapiro.test(aci_accessions$mean_Asat) #normally distributed 

hist(aci_accessions$mean_gs_Asat)
shapiro.test(aci_accessions$mean_gs_Asat) #normally distributed 

hist(aci_accessions$mean_Alow)
shapiro.test(aci_accessions$mean_Alow) #normally distributed

hist(aci_accessions$mean_gs_Alow)
shapiro.test(aci_accessions$mean_gs_Alow) #normally distributed 

hist(aci_accessions$Jmax)
shapiro.test(aci_accessions$Jmax) #normally distributed

hist(aci_accessions$Vcmax)
shapiro.test(aci_accessions$Vcmax) #normally distributed 

hist(aci_accessions$asat)
shapiro.test(aci_accessions$Vcmax) #normally distributed

hist(aci_accessions$gs)
shapiro.test(aci_accessions$gs) #normally distributed

hist(aci_accessions$iWUE)
shapiro.test(aci_accessions$iWUE) #normally distributed

hist(aci_accessions$sl)
shapiro.test(aci_accessions$sl) #normally distributed 

aci_accessions <- aci_accessions[,-c(14:25)]

#correlation plots and tests for aci accessions ----

names(aci_accessions)[4] <- "high light A"
names(aci_accessions)[5] <- "high light gs"
names(aci_accessions)[6] <- "low light A"
names(aci_accessions)[7] <- "low light gs"
names(aci_accessions)[10] <- "Asat"

aci_matrix <- cor.mtest(aci_accessions[,3:13], conf.level = 0.95)

corrplot(cor(aci_accessions[,c(3:13)]), p.mat = aci_matrix$p, sig.level = 0.05, type = "upper", 
         diag = FALSE, method = "circle", title = "Parameter Correlations for A-Ci Accessions", mar=c(0,0,1,0)) #get crosses for non-significant interactions

rcorr(as.matrix(aci_accessions[,3:13]))

test <- merge(aci_accessions, NPQ, by = "Genotype")

test_matrix <- cor.mtest(test[,3:20], conf.level = 0.95)

corrplot(cor(test[,c(3:20)]), p.mat = test_matrix$p, sig.level = 0.05, type = "upper", 
         diag = FALSE, method = "circle", title = "Parameter Correlations for A-Ci Accessions", mar=c(0,0,1,0)) #get crosses for non-significant interactions

