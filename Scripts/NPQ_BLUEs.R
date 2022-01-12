#get libraries ----

library(readxl)
library(lme4)
library(desplot)
library(emmeans)
library(ggplot2)
library(readxl)
library(tidyverse)
library(inti)

#get data (and plot means) ----

NPQ <- read_excel("~/OneDrive - University of Cambridge/MPhil/GitLink/ExportedData/NPQparms.xlsx")

#get some boxplots like wanne wanted ----

ggplot(NPQ, aes(x=reorder(Name, a_fit, FUN = median), y=a_fit)) + geom_boxplot() +
  xlab("Genotype")

ggplot(NPQ, aes(x=reorder(Name, f_fit, FUN = median), y=f_fit)) + geom_boxplot() +
  xlab("Genotype") + ylab("Phi PSII a")

#try some plots for plot means ----

NPQ_means <- aggregate(NPQ[,3:14], list(NPQ$Name, NPQ$Plot, NPQ$Rep), mean)
names(NPQ_means)[1] <- "Name"
names(NPQ_means)[2] <- "Plot"
names(NPQ_means)[3] <- "Rep"

#get blues/blups----

info <- read_excel("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/CF files/NPQ_additional_info.xlsx")
info <- select(info, Plot, Date)
info <- unique(info)

NPQ_means <- merge(NPQ_means, info, by = "Plot")

#a ----

hr <- H2cal(data = NPQ_means,
            trait = "a_fit",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep) + (1|Date)",
            ran.model = "(1|Name) + (1|Rep) + (1|Date)",
            plot_diag = TRUE)

hr$tabsmr
a_blups <- hr$blups
names(a_blups)[2] <- "a_blups"

ggplot(a_blups, aes(x=a_blups)) + geom_density(color="darkblue", fill="lightblue") + xlab("a (induction) BLUPs") +
  ylab("Density") + theme_classic()

#b ----

hr <- H2cal(data = NPQ_means,
            trait = "b_fit",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep) + (1|Date)",
            ran.model = "(1|Name) + (1|Rep) + (1|Date)",
            plot_diag = TRUE)

hr$tabsmr
b_blups <- hr$blups
names(b_blups)[2] <- "b_blups"

ggplot(b_blups, aes(x=b_blups)) + geom_density(color="darkblue", fill="lightblue") + xlab("b (induction) BLUPs") +
  ylab("Density") + theme_classic()

#c ----

hr <- H2cal(data = NPQ_means,
            trait = "c_fit",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep) + (1|Date)",
            ran.model = "(1|Name) + (1|Rep) + (1|Date)",
            plot_diag = TRUE)

hr$tabsmr
c_blups <- hr$blups
names(c_blups)[2] <- "c_blups"

ggplot(c_blups, aes(x=c_blups)) + geom_density(color="darkblue", fill="lightblue") + xlab("c (induction) BLUPs") +
  ylab("Density") + theme_classic()

#d ----

hr <- H2cal(data = NPQ_means,
            trait = "d_fit",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep) + (1|Date)",
            ran.model = "(1|Name) + (1|Rep) + (1|Date)",
            plot_diag = TRUE)

hr$tabsmr
d_blups <- hr$blups
names(d_blups)[2] <- "d_blups"

ggplot(d_blups, aes(x=d_blups)) + geom_density(color="darkblue", fill="lightblue") + xlab("d (induction) BLUPs") +
  ylab("Density") + theme_classic()


#e ----

hr <- H2cal(data = NPQ_means,
            trait = "e_fit",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep) + (1|Date)",
            ran.model = "(1|Name) + (1|Rep) + (1|Date)",
            plot_diag = TRUE)

hr$tabsmr
e_blups <- hr$blups
names(e_blups)[2] <- "e_blups"

ggplot(e_blups, aes(x=e_blups)) + geom_density(color="darkblue", fill="lightblue") + xlab("e (induction) BLUPs") +
  ylab("Density") + theme_classic()

#f ----

hr <- H2cal(data = NPQ_means,
            trait = "f_fit",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep) + (1|Date)",
            ran.model = "(1|Name) + (1|Rep) + (1|Date)",
            plot_diag = TRUE)

hr$tabsmr
f_blups <- hr$blups
names(f_blups)[2] <- "f_blups"

ggplot(f_blups, aes(x=f_blups)) + geom_density(color="darkblue", fill="lightblue") + xlab("a phi PSII in dark BLUPs") +
  ylab("Density") + theme_classic()


#g ----

hr <- H2cal(data = NPQ_means,
            trait = "g_fit",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep) + (1|Date)",
            ran.model = "(1|Name) + (1|Rep) + (1|Date)",
            plot_diag = TRUE)

hr$tabsmr
g_blups <- hr$blups
names(g_blups)[2] <- "g_blups"

#h ----

hr <- H2cal(data = NPQ_means,
            trait = "h_fit",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep) + (1|Date)",
            ran.model = "(1|Name) + (1|Rep) + (1|Date)",
            plot_diag = TRUE)

hr$tabsmr
h_blups <- hr$blups
names(h_blups)[2] <- "h_blups"

#max amp ----

hr <- H2cal(data = NPQ_means,
            trait = "max_amp",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep) + (1|Date)",
            ran.model = "(1|Name) + (1|Rep) + (1|Date)",
            plot_diag = TRUE)

hr$tabsmr
max_amp_blups <- hr$blups
names(max_amp_blups)[2] <- "max_amp_blups"

#initial slope ----

hr <- H2cal(data = NPQ_means,
            trait = "gradient",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep) + (1|Date)",
            ran.model = "(1|Name) + (1|Rep) + (1|Date)",
            plot_diag = TRUE)

hr$tabsmr
gradient_blups <- hr$blups
names(gradient_blups)[2] <- "gradient_blups"

#end NPQ ----

hr <- H2cal(data = NPQ_means,
            trait = "end_NPQ",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep) + (1|Date)",
            ran.model = "(1|Name) + (1|Rep) + (1|Date)",
            plot_diag = TRUE)

hr$tabsmr
end_NPQ_blups <- hr$blups
names(end_NPQ_blups)[2] <- "end_NPQ_blups"

#end FvFm ---- 

hr <- H2cal(data = NPQ_means,
            trait = "end_FvFm",
            gen.name = "Name",
            rep.n = 2,
            fix.model = "0 + Name + (1|Rep) + (1|Date)",
            ran.model = "(1|Name) + (1|Rep) + (1|Date)",
            plot_diag = TRUE)

hr$tabsmr
end_FvFm_blups <- hr$blups
names(end_FvFm_blups)[2] <- "end_FvFm_blups"

#NPQ blups ----

NPQ_blups <- list(end_NPQ_blups, max_amp_blups, gradient_blups, a_blups, b_blups, c_blups,
     d_blups, e_blups, f_blups, g_blups, h_blups, end_FvFm_blups) %>% reduce(left_join, by="Name")

write.csv(NPQ_blups, "~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign/BLUPs for PCA/NPQ_blups.csv")
