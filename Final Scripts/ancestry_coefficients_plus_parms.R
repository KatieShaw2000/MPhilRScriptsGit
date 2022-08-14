#set working directory ----

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign")

#get packages needed ---- 

library(readxl)
library(dunn.test)
library(ggplot2)
library(ggsignif)    
library(ggpubr)

#get data ----

ancestry <- read_excel("final_ancestry_coefficients.xlsx")
ancestry <- ancestry[,c(1,6,7)]
names(ancestry)[2] <- "location_type"
names(ancestry)[3] <- "value"

parms <- read.csv("~/OneDrive - University of Cambridge/MPhil/GitLink/Final Scripts Output/all_blues.csv")
parms <- parms[,-1]

combined <- merge(ancestry, parms, by = "Genotype")

#get rid of data where ancestry coefficient is below 0.6 ----

combined <- subset(combined, value >= 0.6)
combined <- within(combined, location_type <- factor(location_type, levels = c("North", "Coast", "Eastern desert", "Southern desert")))
row.names(combined) <- seq(1,206,1)

combined <- combined[-c(124:128),] #remove site 31

north <- subset(combined, location_type == "North")
coast<- subset(combined, location_type == "Coast")
e_desert <- subset(combined, location_type == "Eastern desert")
s_desert <- subset(combined, location_type == "Southern desert")

#heading date ----

hist(combined$days_to_heading)
shapiro.test(combined$days_to_heading) #not normal 
bartlett.test(days_to_heading ~ location_type, data=combined) #not equal variance --> use kruskal wallis

lm_hd <- lm(days_to_heading ~ location_type, data = combined)
shapiro.test(residuals(lm_hd)) #not normal residuals --> use kruskal wallis

kruskal.test(days_to_heading ~ location_type, data = combined)
dunn.test(combined$days_to_heading, combined$location_type, altp = TRUE)

hd <- ggplot(combined, aes(x=location_type, y=days_to_heading, color=location_type)) + geom_boxplot() +
  ylab("Days to Heading") + 
  xlab("Ancestral Group") +
  guides(color = guide_legend(title = "Ancestral Group")) +
  theme(legend.position = c(0.18,0.13)) +
  scale_color_manual(values=c("red","blue", "green4", "darkorange")) +
  geom_signif(comparisons = list(c("North", "Eastern desert"),
                                 c("Coast", "Eastern desert"),
                                 c("Eastern desert", "Southern desert"),
                                 c("North", "Southern desert")),map_signif_level = TRUE,
              y_position = c(43,41,39,45), color = "black")

#SLA ----

hist(combined$SLA)
shapiro.test(combined$SLA) #normal
bartlett.test(SLA ~ location_type, data=combined) #equal variance

lm_SLA <- lm(SLA ~ location_type, data = combined)
shapiro.test(residuals(lm_SLA)) #normal --> use ANOVA

aov_SLA <- aov(formula = SLA ~ location_type, data = combined)
summary(aov_SLA)
TukeyHSD(aov_SLA)

SLA <- ggplot(combined, aes(x=location_type, y=SLA, color=location_type)) + geom_boxplot() +
  ylab(expression(paste("SLA (m"^"2","g"^"-1",")"))) +
  theme(legend.position = "none") +
  xlab("Ancestral Group") +
  scale_color_manual(values=c("red","blue", "green4", "darkorange")) +
  geom_signif(comparisons = list(c("North", "Eastern desert"),
                                 c("Coast", "Eastern desert"),
                                 c("Eastern desert", "Southern desert")),
              map_signif_level = TRUE,
              y_position = c(0.031,0.0304,0.0298), color = "black")

#plot for SLA and heading date ----

ggarrange(hd, SLA, ncol = 2, labels = c("A", "B"))

median(north$days_to_heading)
median(e_desert$days_to_heading)

mean(e_desert$SLA)
mean(s_desert$SLA)
mean(north$SLA)
mean(coast$SLA)

#NPQ parms induction ----

lm_a <- lm(a_fit ~ location_type, data = combined)
shapiro.test(residuals(lm_a)) #normal --> use ANOVA
bartlett.test(a_fit ~ location_type, data=combined) #equal variance --> use ANOVA

aov_a <- aov(a_fit ~ location_type, data = combined)
summary(aov_a) #significant difference
TukeyHSD(aov_a)

a <- ggplot(combined, aes(x=location_type, y=a_fit, color=location_type)) + geom_boxplot(lwd=1) +
  ylab("a_induction") +
  theme(legend.position = "none") +
  xlab("Ancestral Group") +
  theme(axis.title = element_text(size=15), axis.text = element_text(size=12)) +
  scale_color_manual(values=c("red","blue", "green4", "darkorange")) +
  geom_signif(comparisons = list(c("Coast", "Eastern desert")),
              map_signif_level = TRUE, color = "black", size = 1, textsize = 5)

lm_b <- lm(b_fit ~ location_type, data = combined)
shapiro.test(residuals(lm_b)) #not normal --> use kruskal wallis
bartlett.test(b_fit ~ location_type, data=combined) #not equal variance

kruskal.test(b_fit ~ location_type, data = combined) #no significant difference 

b <- ggplot(combined, aes(x=location_type, y=b_fit, color=location_type)) + geom_boxplot(lwd=1) +
  ylab(expression(paste("b_induction (s"^"-1",")"))) +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size=15), axis.text = element_text(size=12)) +
  xlab("Ancestral Group") +
  scale_color_manual(values=c("red","blue", "green4", "darkorange")) 

lm_max_amp <- lm(max_amp ~ location_type, data = combined)
shapiro.test(residuals(lm_max_amp)) #normal --> use ANOVA
bartlett.test(max_amp ~ location_type, data=combined) #equal variance --> use ANOVA

aov_max_amp <- aov(max_amp ~ location_type, data = combined)
summary(aov_max_amp) #significant difference 
TukeyHSD(aov_max_amp)

max_amp <- ggplot(combined, aes(x=location_type, y=max_amp, color=location_type)) + geom_boxplot(lwd=1) +
  ylab("Maximum NPQ Amplitude") +
  guides(color = guide_legend(title = "Ancestral Group")) +
  theme(legend.position = c(0.87,0.93)) +
  xlab("Ancestral Group") +
  theme(axis.title = element_text(size=15), axis.text = element_text(size=12)) +
  scale_color_manual(values=c("red","blue", "green4", "darkorange")) +
  geom_signif(comparisons = list(c("North", "Eastern desert"),
                                 c("Coast", "Eastern desert")),
              map_signif_level = TRUE,
              y_position = c(4.6,4.5), color = "black", size = 1, textsize = 5)

ggarrange(a, b, max_amp, ncol = 3, labels = c("A", "B", "C"))

#NPQ parms relaxation ----

lm_c <- lm(c_fit ~ location_type, data = combined)
shapiro.test(residuals(lm_c)) #normal --> use ANOVA
bartlett.test(c_fit ~ location_type, data=combined) #equal variance --> use ANOVA

aov_c <- aov(c_fit ~ location_type, data = combined)
summary(aov_c) #significant difference
TukeyHSD(aov_c)

c <- ggplot(combined, aes(x=location_type, y=c_fit, color=location_type)) + geom_boxplot(lwd=1) +
  ylab("a_relaxation") +
  theme(legend.position = "none") +
  xlab("Ancestral Group") +
  theme(axis.title = element_text(size=15), axis.text = element_text(size=12)) +
  scale_color_manual(values=c("red","blue", "green4", "darkorange")) +
  geom_signif(comparisons = list(c("North", "Eastern desert"),
                                 c("Coast", "Eastern desert")),
              map_signif_level = TRUE,
              y_position = c(4.15,4.05), color = "black", size = 1, textsize = 5) #will need to block out one of the stars in the figure for this!!


lm_c <- lm(d_fit ~ location_type, data = combined)
shapiro.test(residuals(lm_d)) #not normal --> kruskal wallis
bartlett.test(d_fit ~ location_type, data=combined) #not equal variance

kruskal.test(d_fit ~ location_type, data = combined) #significant difference
dunn.test(combined$d_fit, combined$location_type, altp = TRUE)

d <- ggplot(combined, aes(x=location_type, y=d_fit, color=location_type)) + geom_boxplot(lwd=1) +
  ylab(expression(paste("b_relaxation (s"^"-1",")"))) +
  theme(legend.position = "none") +
  xlab("Ancestral Group") +
  theme(axis.title = element_text(size=15), axis.text = element_text(size=12)) +
  scale_color_manual(values=c("red","blue", "green4", "darkorange")) +
  geom_signif(comparisons = list(c("North", "Eastern desert"),
                                 c("North", "Southern desert"),
                                 c("North", "Coast")),
              map_signif_level = TRUE,
              y_position = c(0.039,0.041,0.037), color = "black", size = 1, textsize = 5) #will need to block out one of the stars in the figure for this!!

lm_e <- lm(e_fit ~ location_type, data = combined)
shapiro.test(residuals(lm_e)) #not normal --> use kruskal wallis
bartlett.test(e_fit ~ location_type, data=combined) #equal variance

kruskal.test(e_fit ~ location_type, data = combined) #no significant difference

e <- ggplot(combined, aes(x=location_type, y=e_fit, color=location_type)) + geom_boxplot(lwd=1) +
  ylab("c_relaxation") +
  guides(color = guide_legend(title = "Ancestral Group")) +
  theme(legend.position = c(0.09,0.845)) +
  xlab("Ancestral Group") +
  theme(axis.title = element_text(size=15), axis.text = element_text(size=12)) +
  scale_color_manual(values=c("red","blue", "green4", "darkorange")) 

lm_end_NPQ <- lm(end_NPQ ~ location_type, data = combined)
shapiro.test(residuals(lm_end_NPQ)) #normal --> use ANOVA
bartlett.test(end_NPQ ~ location_type, data=combined) #equal variance

aov_end_NPQ <- aov(end_NPQ ~ location_type, data = combined)
summary(aov_end_NPQ) #no significant difference

end_NPQ <- ggplot(combined, aes(x=location_type, y=end_NPQ, color=location_type)) + geom_boxplot(lwd=1) +
  ylab("End NPQ") +
  theme(legend.position = "none") +
  xlab("Ancestral Group") +
  theme(axis.title = element_text(size=15), axis.text = element_text(size=12)) +
  scale_color_manual(values=c("red","blue", "green4", "darkorange")) 

ggarrange(c, d, e, end_NPQ, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"))

#water-related parameters ----

aci_parms <- combined[,c(1:3,10:15)]
aci_parms <- na.omit(aci_parms)
row.names(aci_parms) <- seq(1,28,1)
aci_parms <- aci_parms[-c(1,9,21),]

aci_desert <- subset(aci_parms, location_type == "Eastern desert")
aci_north <- subset(aci_parms, location_type == "North")
aci_coast <- subset(aci_parms, location_type == "Coast")

lm_gs <- lm(gs ~ location_type, data = aci_parms)
shapiro.test(residuals(lm_gs)) #normal --> use ANOVA
bartlett.test(gs ~ location_type, data=aci_parms) #equal variance --> use ANOVA

aov_gs <- aov(gs ~ location_type, data = aci_parms)
summary(aov_gs) #significant difference
TukeyHSD(aov_gs)

gs <- ggplot(aci_parms, aes(x=location_type, y=gs, color=location_type)) + geom_boxplot(lwd=1) +
  ylab(expression(paste("gs (",mol,~ m^2, s^-1,")"))) +
  theme(legend.position = "none") +
  xlab("Ancestral Group") +
  theme(axis.title = element_text(size=15), axis.text = element_text(size=12)) +
  scale_color_manual(values=c("red","blue", "green4")) +
  geom_signif(comparisons = list(c("North", "Eastern desert")),
            map_signif_level = TRUE,
            y_position = c(0.7), color = "black", size = 1, textsize = 5) #need to block out a star!


lm_iwue <- lm(iWUE ~ location_type, data = aci_parms)
shapiro.test(residuals(lm_iwue)) #normal --> use ANOVA
bartlett.test(iWUE~ location_type, data=aci_parms) #equal variance --> use ANOVA

aov_iwue <- aov(iWUE ~ location_type, data = aci_parms)
summary(aov_iwue) #significant difference
TukeyHSD(aov_iwue)

iWUE <- ggplot(aci_parms, aes(x=location_type, y=iWUE, color=location_type)) + geom_boxplot(lwd=1) +
  theme(legend.position = "none") +
  ylab(expression(paste("iWUE (", mu, "mol ", "mol"^"-1",")"))) +
  xlab("Ancestral Group") +
  theme(axis.title = element_text(size=15), axis.text = element_text(size=12)) +
  scale_color_manual(values=c("red","blue", "green4")) +
  geom_signif(comparisons = list(c("North", "Eastern desert")),
              map_signif_level = TRUE,
              y_position = c(100), color = "black", size = 1, textsize = 5)

lm_sl <- lm(sl ~ location_type, data = aci_parms)
shapiro.test(residuals(lm_sl)) #normal --> use ANOVA
bartlett.test(sl ~ location_type, data=aci_parms) #equal variance --> use ANOVA

aov_sl <- aov(sl ~ location_type, data = aci_parms)
summary(aov_sl) #no significant difference 

sl <- ggplot(aci_parms, aes(x=location_type, y=sl, color=location_type)) + geom_boxplot(lwd=1) +
  ylab("sl") +
  guides(color = guide_legend(title = "Ancestral Group")) +
  theme(legend.position = c(0.87,0.07)) +
  xlab("Ancestral Group") +
  theme(axis.title = element_text(size=15), axis.text = element_text(size=12)) +
  scale_color_manual(values=c("red","blue", "green4", "darkorange")) 

ggarrange(gs, iWUE, sl, ncol = 3, labels = c("A", "B", "C"))

#photosynthetic parameters----

lm_asat <- lm(asat~ location_type, data = aci_parms)
shapiro.test(residuals(lm_asat)) #normal --> use ANOVA
bartlett.test(asat ~ location_type, data=aci_parms) #equal variance --> use ANOVA

aov_asat <- aov(asat ~ location_type, data = aci_parms)
summary(aov_asat) #no significant difference

asat <- ggplot(aci_parms, aes(x=location_type, asat, color=location_type)) + geom_boxplot(lwd=1) +
  ylab(expression(paste("Asat (",mu, "mol",~ m^2, s^-1,")"))) +
  theme(legend.position = "none") +
  xlab("Ancestral Group") +
  theme(axis.title = element_text(size=15), axis.text = element_text(size=12)) +
  scale_color_manual(values=c("red","blue", "green4")) 

lm_jmax <- lm(Jmax ~ location_type, data = aci_parms)
shapiro.test(residuals(lm_jmax)) #normal --> use ANOVA
bartlett.test(Jmax~ location_type, data=aci_parms) #equal variance --> use ANOVA

aov_jmax<- aov(Jmax ~ location_type, data = aci_parms)
summary(aov_jmax) #no significant difference

jmax <- ggplot(aci_parms, aes(x=location_type, Jmax, color=location_type)) + geom_boxplot(lwd=1) +
  ylab(expression(paste("Jmax (",mu, "mol",~ m^2, s^-1,")"))) +
  theme(legend.position = "none") +
  xlab("Ancestral Group") +
  theme(axis.title = element_text(size=15), axis.text = element_text(size=12)) +
  scale_color_manual(values=c("red","blue", "green4")) 

lm_vcmax <- lm(Vcmax ~ location_type, data = aci_parms)
shapiro.test(residuals(lm_vcmax)) #normal --> use ANOVA
bartlett.test(Vcmax~ location_type, data=aci_parms) #equal variance --> use ANOVA

aov_vcmax<- aov(Vcmax ~ location_type, data = aci_parms)
summary(aov_vcmax) #no significant difference

vcmax <- ggplot(aci_parms, aes(x=location_type, Vcmax, color=location_type)) + geom_boxplot(lwd=1) +
  ylab(expression(paste("Vcmax (",mu, "mol",~ m^2, s^-1,")"))) +
  theme(legend.position = "none") +
  xlab("Ancestral Group") +
  theme(axis.title = element_text(size=15), axis.text = element_text(size=12)) +
  scale_color_manual(values=c("red","blue", "green4")) 

lm_6400_asat <- lm(mean_Asat ~ location_type, data = combined)
shapiro.test(residuals(lm_6400_asat)) #normal --> use ANOVA
bartlett.test(mean_Asat ~ location_type, data=combined) #equal variance --> use ANOVA

aov_6400_asat <- aov(mean_Asat ~ location_type, data = combined)
summary(aov_6400_asat) #significant difference
TukeyHSD(aov_6400_asat)

asat_6400 <- ggplot(combined, aes(x=location_type, mean_Asat, color=location_type)) + geom_boxplot(lwd=1) +
  ylab(expression(paste("A in High Light (",mu, "mol",~ m^2, s^-1,")"))) +
  guides(color = guide_legend(title = "Ancestral Group")) +
  theme(legend.position = c(0.78,0.78)) +
  xlab("Ancestral Group") +
  theme(axis.title = element_text(size=15), axis.text = element_text(size=12)) +
  scale_color_manual(values=c("red","blue", "green4", "darkorange")) +
  geom_signif(comparisons = list(c("North", "Southern desert"),
                               c("North", "Eastern desert"),
                               c("Coast", "Eastern desert")),
            map_signif_level = TRUE,
            y_position = c(35,34,33), color = "black", size = 1, textsize = 5)

ggarrange(asat_6400, asat, vcmax, jmax, ncol=4, widths = c(1.5,1,1,1), labels = c("A", "B", "C", "D"))
