#set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/GitLink/Final Scripts Output")

#get libraries ----

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

#get data ----

aci <- read.csv("aci_blues.csv")
aci <- aci[,-1]

SLA <- read.csv("SLA_blues.csv")
SLA <- SLA[,-1]

hd <- read.csv("heading_date_blues.csv")
hd <- hd[,-1]

licor_6400 <- read.csv("6400_blues.csv")
licor_6400 <- licor_6400[,-1]

NPQ <- read.csv("NPQ_blues.csv")
NPQ <- NPQ[,-1]

#merge into one dataframe ----

all_blues <- list(hd, SLA, licor_6400, aci, NPQ) %>%
  reduce(left_join, by = "Genotype")

write.csv(all_blues, "all_blues.csv")

#get some density plots ----

rm(aci, hd, NPQ, licor_6400, SLA)

hd <- ggplot(data=all_blues, aes(x=days_to_heading)) +
  ylab("Density") +
  xlab("Days to Heading") +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 37.5, y = 0.3, label = "0.854")
  
a_high_light <- ggplot(data=all_blues, aes(x=mean_Asat)) +
  ylab("Density") +
  xlab(expression(paste("A in High Light (",mu, "mol",~ m^2, s^-1,")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 30, y = 0.07, label = "0.562")

alow_light <- ggplot(data=all_blues, aes(x=mean_Alow)) +
  ylab("Density") +
  xlim(0,13) +
  xlab(expression(paste("A in Low Light (",mu, "mol",~ m^2, s^-1,")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 2.5, y = 0.2, label = "0.242")

gs_high_light <- ggplot(data=all_blues, aes(x=mean_gs_Asat)) +
  ylab("Density") +
  xlab(expression(paste("gs in High Light (",mol,~ m^2, s^-1,")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 0.45, y = 4, label = "0.566")

gs_low_light <- ggplot(data=all_blues, aes(x=mean_gs_Alow)) +
  ylab("Density") +
  xlab(expression(paste("gs in Low Light (",mol,~ m^2, s^-1,")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 0.35, y = 5, label = "0.550")

asat <- ggplot(data=all_blues, aes(x=asat)) +
  ylab("Density") +
  xlab(expression(paste("asat (",mu, "mol",~ m^2, s^-1,")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 30, y = 0.075, label = "0.570")

vcmax <- ggplot(data=all_blues, aes(x=Vcmax)) +
  ylab("Density") +
  xlab(expression(paste("Vcmax (",mu, "mol",~ m^2, s^-1,")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 110, y = 0.025, label = "0.596")

jmax <- ggplot(data=all_blues, aes(x=Jmax)) +
  ylab("Density") +
  xlab(expression(paste("Jmax (",mu, "mol",~ m^2, s^-1,")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 212.5, y = 0.0125, label = "0.567")

gs <- ggplot(data=all_blues, aes(x=gs)) +
  ylab("Density") +
  xlab(expression(paste("gs (",mol,~ m^2, s^-1,")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 0.6, y = 3, label = "0.306")

iwue <- ggplot(data=all_blues, aes(x=iWUE)) +
  ylab("Density") +
  xlab(expression(paste("iWUE (", mu, "mol ", "mol",~ m^2, s^-1,")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 90, y = 0.035, label = "0.517")

sl <- ggplot(data=all_blues, aes(x=sl)) +
  ylab("Density") +
  xlab("sl") +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 0.1, y = 6, label = "0.611")

sla <- ggplot(data=all_blues, aes(x=SLA)) +
  ylab("Density") +
  xlab(expression(paste("SLA (cm"^"2","g"^"-1",")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 280, y = 0.015, label = "0.620")

a <- ggplot(data=all_blues, aes(x=a_fit)) +
  ylab("Density") +
  xlab("a_induction") +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 3, y = 1, label = "0.668")

b <- ggplot(data=all_blues, aes(x=b_fit)) +
  ylab("Density") +
  xlab("b_induction") +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 0.012, y = 200, label = "0.536")

c <- ggplot(data=all_blues, aes(x=c_fit)) +
  ylab("Density") +
  xlab("a_relaxation") +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 3.75, y = 1, label = "0.672")

d <- ggplot(data=all_blues, aes(x=d_fit)) +
  ylab("Density") +
  xlab("b_relaxation") +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 0.03, y = 90, label = "0.664")

e <- ggplot(data=all_blues, aes(x=e_fit)) +
  ylab("Density") +
  xlab("c_relaxation") +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 0.6, y = 6, label = "0.540")

max_amp <- ggplot(data=all_blues, aes(x=max_amp)) +
  ylab("Density") +
  xlab("Maximum NPQ Amplitude") +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 4.25, y = 1, label = "0.647")

end_npq <- ggplot(data=all_blues, aes(x=end_NPQ)) +
  ylab("Density") +
  xlab("End NPQ") +
  geom_density(color = "darkblue", fill = "lightblue")+
  annotate("text", x = 0.45, y = 7, label = "0.492")

ggarrange(hd, a_high_light, alow_light, gs_high_light, gs_low_light, 
          asat, vcmax, jmax, gs, iwue, sl, sla, a, b, max_amp, c, d, e, end_npq, ncol=5, nrow=4)


