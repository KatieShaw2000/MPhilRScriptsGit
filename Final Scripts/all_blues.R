#set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/GitLink/Final Scripts Output")

#get libraries ----

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(purrr)

#get data ----

aci <- read.csv("aci_blues.csv")
aci <- aci[,-1]

SLA <- read.csv("SLA_blues.csv")
SLA <- SLA[,-1]
SLA$SLA <- SLA$SLA/100
SLA$SLA <- SLA$SLA/100

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
  annotate("text", x = 37.5, y = 0.3, label = "0.854 (n=320)")
  
a_high_light <- ggplot(data=all_blues, aes(x=mean_Asat)) +
  ylab("Density") +
  xlab(expression(paste("A in High Light (",mu, "mol",~ m^2, s^-1,")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 12.5, y = 0.06, label = "0.562 (n=320)")

alow_light <- ggplot(data=all_blues, aes(x=mean_Alow)) +
  ylab("Density") +
  xlim(0,13) +
  xlab(expression(paste("A in Low Light (",mu, "mol",~ m^2, s^-1,")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 2.5, y = 0.2, label = "0.510 (n=320)")

gs_high_light <- ggplot(data=all_blues, aes(x=mean_gs_Asat)) +
  ylab("Density") +
  xlab(expression(paste("gs in High Light (",mol,~ m^2, s^-1,")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 0.45, y = 4, label = "0.566 (n=320)")

gs_low_light <- ggplot(data=all_blues, aes(x=mean_gs_Alow)) +
  ylab("Density") +
  xlab(expression(paste("gs in Low Light (",mol,~ m^2, s^-1,")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 0.35, y = 5, label = "0.549 (n=320)")

asat <- ggplot(data=all_blues, aes(x=asat)) +
  ylab("Density") +
  xlab(expression(paste("Asat (",mu, "mol",~ m^2, s^-1,")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 20, y = 0.085, label = "0.570 (n=47)")

vcmax <- ggplot(data=all_blues, aes(x=Vcmax)) +
  ylab("Density") +
  xlab(expression(paste("Vcmax (",mu, "mol",~ m^2, s^-1,")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 110, y = 0.025, label = "0.596 (n=47)")

jmax <- ggplot(data=all_blues, aes(x=Jmax)) +
  ylab("Density") +
  xlab(expression(paste("Jmax (",mu, "mol",~ m^2, s^-1,")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 212.5, y = 0.0125, label = "0.567 (n=47)")

gs <- ggplot(data=all_blues, aes(x=gs)) +
  ylab("Density") +
  xlab(expression(paste("gs (",mol,~ m^2, s^-1,")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 0.6, y = 3, label = "0.306 (n=47)")

iwue <- ggplot(data=all_blues, aes(x=iWUE)) +
  ylab("Density") +
  xlab(expression(paste("iWUE (", mu, "mol ", "mol"^"-1",")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 90, y = 0.035, label = "0.517 (n=47)")


sl <- ggplot(data=all_blues, aes(x=sl)) +
  ylab("Density") +
  xlab("sl") +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 0.1, y = 6, label = "0.611 (n=47)")

sla <- ggplot(data=all_blues, aes(x=SLA)) +
  ylab("Density") +
  xlab(expression(paste("SLA (m"^"2","g"^"-1",")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 0.028, y = 150, label = "0.620 (n=320)")

a <- ggplot(data=all_blues, aes(x=a_fit)) +
  ylab("Density") +
  xlab("a_induction") +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 3, y = 1, label = "0.668 (n=315)")

b <- ggplot(data=all_blues, aes(x=b_fit)) +
  ylab("Density") +
  xlab(expression(paste("b_induction (s"^"-1",")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 0.012, y = 200, label = "0.536 (n=315)")

c <- ggplot(data=all_blues, aes(x=c_fit)) +
  ylab("Density") +
  xlab("a_relaxation") +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 3.8, y = 1, label = "0.672 (n=315)")

d <- ggplot(data=all_blues, aes(x=d_fit)) +
  ylab("Density") +
  xlab(expression(paste("b_relaxation (s"^"-1",")"))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 0.03, y = 90, label = "0.664 (n=315)")

e <- ggplot(data=all_blues, aes(x=e_fit)) +
  ylab("Density") +
  xlab("c_relaxation") +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 0.6, y = 6, label = "0.540 (n=315)")

max_amp <- ggplot(data=all_blues, aes(x=max_amp)) +
  ylab("Density") +
  xlab("Maximum NPQ Amplitude") +
  geom_density(color = "darkblue", fill = "lightblue") +
  annotate("text", x = 4.25, y = 1, label = "0.647 (n=315)")

end_npq <- ggplot(data=all_blues, aes(x=end_NPQ)) +
  ylab("Density") +
  xlab("End NPQ") +
  geom_density(color = "darkblue", fill = "lightblue")+
  annotate("text", x = 0.45, y = 7, label = "0.492 (n=315)")

ggarrange(hd, a_high_light, alow_light, gs_high_light, gs_low_light, 
          asat, vcmax, jmax, gs, iwue, sl, sla, a, b, max_amp, c, d, e, end_npq, ncol=5, nrow=4)


