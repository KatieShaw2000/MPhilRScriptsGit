#packages ----

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
library(ggforce)
library(forcats)
library(plyr)
library(lme4)
library(inti)
library(readxl)
library(spectratrait)
library(hyperSpec)

#set working directory ---- 

setwd("~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData")

#get data ---- 

hyperspectral <- read_csv("hyperspectral.csv")
hyperspectral <- hyperspectral[,-1]

#plot the raw data ----

# hyperspectral <- hyperspectral[!(hyperspectral$Repeat_two == "0"),] #remove the ones where repeat_2 is 0
# 
# long_hyperspectral <- hyperspectral %>%
#   gather(Wavelength, Data, -Date, -Plot, -Repeat, -Repeat_two)
# 
# PDFpath1_1 <- "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/Hyperspectral files/Repeat1_raw.pdf"
# pdf(file=PDFpath1_1)
# 
# Repeat1 <- subset(long_hyperspectral, long_hyperspectral$Repeat == 1)
# 
# for (value in unique(Repeat1$Plot)){
#   subset <- subset(Repeat1, Repeat1$Plot == value)
#   plot(subset$Wavelength, subset$Data, col='Black', xlim=c(350,2500), ylim=c(0,0.8), 
#        main=paste("Plot of", value,"_ 1"), xlab="Wavelength")
# }
# 
# dev.off()
# 
# PDFpath2_1 <- "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/Hyperspectral files/Repeat2_raw.pdf"
# pdf(file=PDFpath2_1)
# 
# Repeat2 <- subset(long_hyperspectral, long_hyperspectral$Repeat == 2)
# 
# for (value in unique(Repeat2$Plot)){
#   subset <- subset(Repeat2, Repeat2$Plot == value)
#   plot(subset$Wavelength, subset$Data, col='Black', xlim=c(350,2500), ylim=c(0,0.8), 
#        main=paste("Plot of", value,"_ 2"), xlab="Wavelength")
# }
# 
# dev.off()
# 
# PDFpath3_1 <- "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/Hyperspectral files/Repeat3_raw.pdf"
# pdf(file=PDFpath3_1)
# 
# Repeat3 <- subset(long_hyperspectral, long_hyperspectral$Repeat == 3)
# 
# for (value in unique(Repeat3$Plot)){
#   subset <- subset(Repeat3, Repeat3$Plot == value)
#   plot(subset$Wavelength, subset$Data, col='Black', xlim=c(350,2500), ylim=c(0,0.8), 
#        main=paste("Plot of", value,"_ 3"), xlab="Wavelength")
# }
# 
# dev.off()

#get means ----

hyperspectral_means <- aggregate(hyperspectral[, 5:2155], list(hyperspectral$Plot, hyperspectral$Repeat), mean)
names(hyperspectral_means)[1] <- "Plot"
names(hyperspectral_means)[2] <- "Repeat"

rm(hyperspectral)

#get Jmax/Vcmax data---- 

parms <- read_excel("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/LICOR 6800 files/red_aci_coef.xlsx")
parms <- parms[,-c(1,4)]

inVar <- "Vcmax"

combined_data <- merge(parms, hyperspectral_means, by = c("Plot", "Repeat"))

rm(parms)

#get names for spectra like you did in Angie's dataset 

Start.wave <- 350
End.wave <- 2500
wv <- seq(Start.wave, End.wave,1)
Spectra <- as.matrix(combined_data[,names(combined_data) %in% wv])
colnames(Spectra) <- c(paste0("Wave_", wv))

sample_info <- combined_data[,1:4]

plsr_data <- data.frame(sample_info, Spectra)
rm(sample_info, Spectra, combined_data, hyperspectral_means)

#create calibration and validation datasets ----

method <- "dplyr"
split_data <- spectratrait::create_data_split(dataset=plsr_data, approach=method, split_seed=7529075, prop=0.7, group_variables="Plot")
cal.plsr.data <- split_data$cal_data
val.plsr.data <- split_data$val_data

rm(split_data)

write.csv(cal.plsr.data, "calibration_data.csv")
write.csv(val.plsr.data, "validation_data.csv")

#check to see if Vcmax/Jmax values in each cal/val datasets are normal - I think these look pretty fine

ggplot(cal.plsr.data, aes(x=Jmax)) + geom_histogram()
ggplot(cal.plsr.data, aes(x=Vcmax)) + geom_histogram()

ggplot(val.plsr.data, aes(x=Jmax)) + geom_histogram()
ggplot(val.plsr.data, aes(x=Vcmax)) + geom_histogram()

#get data in format as in github step 8 ----

`%notin%` <- Negate(`%in%`)

cal_spec <- as.matrix(cal.plsr.data[, which(names(cal.plsr.data) %in% 
                                              paste0("Wave_",wv))])
cal.plsr.data <- data.frame(cal.plsr.data[, which(names(cal.plsr.data) %notin% 
                                                    paste0("Wave_",wv))], 
                            Spectra=I(cal_spec))
head(cal.plsr.data)[1:4]

val_spec <- as.matrix(val.plsr.data[, which(names(val.plsr.data) %in% 
                                              paste0("Wave_",wv))])
val.plsr.data <- data.frame(val.plsr.data[, which(names(val.plsr.data) %notin% 
                                                    paste0("Wave_",wv))],
                            Spectra=I(val_spec))
head(val.plsr.data)[1:4]

#plot the calibration and validation data ----

par(mfrow=c(1,2)) # B, L, T, R
spectratrait::f.plot.spec(Z=cal.plsr.data$Spectra,wv=wv,
                          plot_label="Calibration")
text(550,95,labels = "2.",cex=3)
spectratrait::f.plot.spec(Z=val.plsr.data$Spectra,wv=wv,
                          plot_label="Validation")

par(mfrow=c(1,1))

#use permutation to get optimal number of components ---- 

if(grepl("Windows", sessionInfo()$running)){
  pls.options(parallel = NULL)
} else {
  pls.options(parallel = parallel::detectCores()-1)
}

method <- "firstMin" #pls, firstPlateau, firstMin
random_seed <- 7529075
seg <- 80
maxComps <- 16
iterations <- 50
prop <- 0.70
if (method=="pls") {
  nComps <- spectratrait::find_optimal_components(dataset=cal.plsr.data, method=method, 
                                                  maxComps=maxComps, seg=seg, 
                                                  random_seed=random_seed)
  print(paste0("*** Optimal number of components: ", nComps))
} else {
  nComps <- spectratrait::find_optimal_components(dataset=cal.plsr.data, method=method, 
                                                  maxComps=maxComps, iterations=iterations, 
                                                  seg=seg, prop=prop, 
                                                  random_seed=random_seed)
}

#fit final model ----

plsr.out <- plsr(as.formula(paste(inVar,"~","Spectra")),scale=FALSE,ncomp=nComps,
                 validation="LOO",trace=FALSE,data=cal.plsr.data)
fit <- plsr.out$fitted.values[,1,nComps]
pls.options(parallel = NULL)

# External validation fit stats
text_loc <- c(max(RMSEP(plsr.out, newdata = val.plsr.data)$comps),
              RMSEP(plsr.out, newdata = val.plsr.data)$val[1])
par(mfrow=c(1,2)) # B, L, T, R
pls::RMSEP(plsr.out, newdata = val.plsr.data)

plot(pls::RMSEP(plsr.out,estimate=c("test"),newdata = val.plsr.data), main="MODEL RMSEP",
     xlab="Number of Components",ylab="Model Validation RMSEP",lty=1,col="black",cex=1.5,lwd=2)
text(text_loc[1],text_loc[2],labels = "4.", cex=2)
box(lwd=2.2)

pls::R2(plsr.out, newdata = val.plsr.data)

plot(pls::R2(plsr.out,estimate=c("test"),newdata = val.plsr.data), main="MODEL R2",
     xlab="Number of Components",ylab="Model Validation R2",lty=1,col="black",cex=1.5,lwd=2)
box(lwd=2.2)

#observed vs predicted fit 

cal.plsr.output <- data.frame(cal.plsr.data[, which(names(cal.plsr.data) %notin% 
                                                      "Spectra")],
                              PLSR_Predicted=fit,
                              PLSR_CV_Predicted=as.vector(plsr.out$validation$pred[,,
                                                                                   nComps]))
cal.plsr.output <- cal.plsr.output %>%
  mutate(PLSR_CV_Residuals = PLSR_CV_Predicted-get(inVar))
head(cal.plsr.output)

cal.R2 <- round(pls::R2(plsr.out,intercept=F)[[1]][nComps],2)
cal.RMSEP <- round(sqrt(mean(cal.plsr.output$PLSR_CV_Residuals^2)),2)

val.plsr.output <- data.frame(val.plsr.data[, which(names(val.plsr.data) %notin% 
                                                      "Spectra")],
                              PLSR_Predicted=as.vector(predict(plsr.out, 
                                                               newdata = val.plsr.data, 
                                                               ncomp=nComps, 
                                                               type="response")[,,1]))
val.plsr.output <- val.plsr.output %>%
  mutate(PLSR_Residuals = PLSR_Predicted-get(inVar))
head(val.plsr.output)

val.R2 <- round(pls::R2(plsr.out,newdata=val.plsr.data,intercept=F)[[1]][nComps],2)
val.RMSEP <- round(sqrt(mean(val.plsr.output$PLSR_Residuals^2)),2)

cal.R2 <- round(pls::R2(plsr.out,newdata=cal.plsr.data,intercept=F)[[1]][nComps],2)
cal.RMSEP <- round(sqrt(mean(cal.plsr.output$PLSR_Residuals^2)),2)

rng_quant <- quantile(cal.plsr.output[,inVar], probs = c(0.001, 0.999))
cal_scatter_plot <- ggplot(cal.plsr.output, aes(x=PLSR_CV_Predicted, y=get(inVar))) + 
  theme_bw() + geom_point() + geom_abline(intercept = 0, slope = 1, color="dark grey", 
                                          linetype="dashed", size=1.5) + 
  xlim(rng_quant[1], rng_quant[2]) + 
  ylim(rng_quant[1], rng_quant[2]) +
  labs(x=paste0("Predicted ", paste(inVar), " (units)"),
       y=paste0("Observed ", paste(inVar), " (units)"),
       title=paste0("Calibration: ", paste0("Rsq = ", cal.R2), "; ", 
                    paste0("RMSEP = ", cal.RMSEP))) +
  theme(axis.text=element_text(size=18), legend.position="none",
        axis.title=element_text(size=20, face="bold"), 
        axis.text.x = element_text(angle = 0,vjust = 0.5),
        panel.border = element_rect(linetype = "solid", fill = NA, size=1.5)) + 
  annotate("text", x=rng_quant[1], y=rng_quant[2], label= "5.",size=10)

cal_resid_histogram <- ggplot(cal.plsr.output, aes(x=PLSR_CV_Residuals)) +
  geom_histogram(alpha=.5, position="identity") + 
  geom_vline(xintercept = 0, color="black", 
             linetype="dashed", size=1) + theme_bw() + 
  theme(axis.text=element_text(size=18), legend.position="none",
        axis.title=element_text(size=20, face="bold"), 
        axis.text.x = element_text(angle = 0,vjust = 0.5),
        panel.border = element_rect(linetype = "solid", fill = NA, size=1.5))

rng_quant <- quantile(val.plsr.output[,inVar], probs = c(0.001, 0.999))
val_scatter_plot <- ggplot(val.plsr.output, aes(x=PLSR_Predicted, y=get(inVar))) + 
  theme_bw() + geom_point() + geom_abline(intercept = 0, slope = 1, color="dark grey", 
                                          linetype="dashed", size=1.5) + 
  xlim(rng_quant[1], rng_quant[2]) + 
  ylim(rng_quant[1], rng_quant[2]) +
  labs(x=paste0("Predicted ", paste(inVar), " (units)"),
       y=paste0("Observed ", paste(inVar), " (units)"),
       title=paste0("Validation: ", paste0("Rsq = ", val.R2), "; ", 
                    paste0("RMSEP = ", val.RMSEP))) +
  theme(axis.text=element_text(size=18), legend.position="none",
        axis.title=element_text(size=20, face="bold"), 
        axis.text.x = element_text(angle = 0,vjust = 0.5),
        panel.border = element_rect(linetype = "solid", fill = NA, size=1.5))

val_resid_histogram <- ggplot(val.plsr.output, aes(x=PLSR_Residuals)) +
  geom_histogram(alpha=.5, position="identity") + 
  geom_vline(xintercept = 0, color="black", 
             linetype="dashed", size=1) + theme_bw() + 
  theme(axis.text=element_text(size=18), legend.position="none",
        axis.title=element_text(size=20, face="bold"), 
        axis.text.x = element_text(angle = 0,vjust = 0.5),
        panel.border = element_rect(linetype = "solid", fill = NA, size=1.5))

# plot cal/val side-by-side
scatterplots <- grid.arrange(cal_scatter_plot, val_scatter_plot, cal_resid_histogram, 
                             val_resid_histogram, nrow=2, ncol=2)
