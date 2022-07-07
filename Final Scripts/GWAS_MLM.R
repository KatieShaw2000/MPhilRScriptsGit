#set working directory ----

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/MPhil/TASSEL")

#get packages ----

library(qqman)
library(dplyr)

# get data ----
tassel <- read.table("MLM_output_B1K_hapmap_KNNimp_+_PC_B1K_hapmap_KNNimp_+_all_blues_stats.txt", header = T, sep = "\t")

# heading date ----

par(mfrow = c(3, 5))   

hd <-  tassel %>% filter(.$Trait == "days_to_heading")
hd <- hd[-1,]
hd <- arrange(hd, p)

hd_7_snps <- hd[1:7,2]

manhattan(hd, chr = "Chr", bp = "Pos", snp = "Marker", p = "p",
  logp = T, 
  #col = c("red", "blue"),
  genomewideline = F, suggestiveline = F, 
  annotateTop = T,
  highlight = hd_7_snps,
  ylim=c(0,6), main = "Days to Heading")

#SLA ----

SLA <-  tassel %>% filter(.$Trait == "SLA")
SLA <- SLA[-1,]
SLA <- arrange(SLA, p)

SLA_7_snps <- SLA[1:7,2]

manhattan(SLA, chr = "Chr", bp = "Pos", snp = "Marker", p = "p",
          logp = T, 
          #col = c("red", "blue"),
          genomewideline = F, suggestiveline = F, 
          annotateTop = T,
          highlight = SLA_7_snps,
          ylim=c(0,5), main = "SLA")

#NPQ induction parameters ----

a_ind <-  tassel %>% filter(.$Trait == "a_fit")
a_ind <- a_ind[-1,]
a_ind <- arrange(a_ind, p)

a_ind_7_snps <- a_ind[1:7,2]

manhattan(a_ind, chr = "Chr", bp = "Pos", snp = "Marker", p = "p",
          logp = T, 
          #col = c("red", "blue"),
          genomewideline = F, suggestiveline = F, 
          annotateTop = T,
          highlight = a_ind_7_snps,
          ylim=c(0,5), main = "a_induction")

b_ind <-  tassel %>% filter(.$Trait == "b_fit")
b_ind <- b_ind[-1,]
b_ind <- arrange(b_ind, p)

b_ind_7_snps <- b_ind[1:7,2]

manhattan(b_ind, chr = "Chr", bp = "Pos", snp = "Marker", p = "p",
          logp = T, 
          #col = c("red", "blue"),
          genomewideline = F, suggestiveline = F, 
          annotateTop = T,
          highlight = b_ind_7_snps,
          ylim=c(0,5), main = "b_induction")

max_amp <-  tassel %>% filter(.$Trait == "max_amp")
max_amp <- max_amp[-1,]
max_amp <- arrange(max_amp, p)

max_amp_7_snps <- max_amp[1:7,2]

manhattan(max_amp, chr = "Chr", bp = "Pos", snp = "Marker", p = "p",
          logp = T, 
          #col = c("red", "blue"),
          genomewideline = F, suggestiveline = F, 
          annotateTop = T,
          highlight = max_amp_7_snps,
          ylim=c(0,5), main = "max_amp")

#NPQ relaxation parameters ----

a_rel <-  tassel %>% filter(.$Trait == "c_fit")
a_rel <- a_rel[-1,]
a_rel <- arrange(a_rel, p)

a_rel_7_snps <- a_rel[1:7,2]

manhattan(a_rel, chr = "Chr", bp = "Pos", snp = "Marker", p = "p",
          logp = T, 
          #col = c("red", "blue"),
          genomewideline = F, suggestiveline = F, 
          annotateTop = T,
          highlight = a_rel_7_snps,
          ylim=c(0,5), main = "a_relaxation")

b_rel <-  tassel %>% filter(.$Trait == "d_fit")
b_rel <- b_rel[-1,]
b_rel <- arrange(b_rel, p)

b_rel_7_snps <- b_rel[1:7,2]

manhattan(b_rel, chr = "Chr", bp = "Pos", snp = "Marker", p = "p",
          logp = T, 
          #col = c("red", "blue"),
          genomewideline = F, suggestiveline = F, 
          annotateTop = T,
          highlight = b_rel_7_snps,
          ylim=c(0,5), main = "b_relaxation")

c_rel <-  tassel %>% filter(.$Trait == "e_fit")
c_rel <- c_rel[-1,]
c_rel <- arrange(c_rel, p)

c_rel_7_snps <- c_rel[1:7,2]

manhattan(c_rel, chr = "Chr", bp = "Pos", snp = "Marker", p = "p",
          logp = T, 
          #col = c("red", "blue"),
          genomewideline = F, suggestiveline = F, 
          annotateTop = T,
          highlight = c_rel_7_snps,
          ylim=c(0,5), main = "c_relaxation")

end_NPQ <-  tassel %>% filter(.$Trait == "end_NPQ")
end_NPQ <- end_NPQ[-1,]
end_NPQ <- arrange(end_NPQ, p)

end_NPQ_7_snps <- end_NPQ[1:7,2]

manhattan(end_NPQ, chr = "Chr", bp = "Pos", snp = "Marker", p = "p",
          logp = T, 
          #col = c("red", "blue"),
          genomewideline = F, suggestiveline = F, 
          annotateTop = T,
          highlight = end_NPQ_7_snps,
          ylim=c(0,5), main = "end_NPQ")

#water related parameters ----

gs <-  tassel %>% filter(.$Trait == "gs")
gs <- gs[-1,]
gs <- arrange(gs, p)

gs_7_snps <- gs[1:7,2]


manhattan(gs, chr = "Chr", bp = "Pos", snp = "Marker", p = "p",
          logp = T, 
          #col = c("red", "blue"),
          genomewideline = F, suggestiveline = F, 
          annotateTop = T,
          highlight = gs_7_snps,
          ylim=c(0,3), main = "gs")

sl <-  tassel %>% filter(.$Trait == "sl")
sl <- sl[-1,]
sl <- arrange(sl, p)

sl_7_snps <- sl[1:7,2]

manhattan(sl, chr = "Chr", bp = "Pos", snp = "Marker", p = "p",
          logp = T, 
          #col = c("red", "blue"),
          genomewideline = F, suggestiveline = F, 
          annotateTop = T,
          highlight = sl_7_snps,
          ylim=c(0,3), main = "sl")

wue <-  tassel %>% filter(.$Trait == "iWUE")
wue <- wue[-1,]
wue <- arrange(wue, p)

wue_7_snps <- wue[1:7,2]


manhattan(wue, chr = "Chr", bp = "Pos", snp = "Marker", p = "p",
          logp = T, 
          #col = c("red", "blue"),
          genomewideline = F, suggestiveline = F, 
          annotateTop = T,
          highlight = wue_7_snps,
          ylim=c(0,3), main = "iWUE")

#photosynthesis A-Ci parameters ----

asat <-  tassel %>% filter(.$Trait == "asat")
asat <- asat[-1,]
asat <- arrange(asat, p)

asat_7_snps <- asat[1:7,2]

manhattan(asat, chr = "Chr", bp = "Pos", snp = "Marker", p = "p",
          logp = T, 
          #col = c("red", "blue"),
          genomewideline = F, suggestiveline = F, 
          annotateTop = T,
          highligh = asat_7_snps,
          ylim=c(0,4), main = "asat")

vcmax <-  tassel %>% filter(.$Trait == "Vcmax")
vcmax <- vcmax[-1,]
vcmax <- arrange(vcmax, p)

vcmax_7_snps <- vcmax[1:7,2]

manhattan(vcmax, chr = "Chr", bp = "Pos", snp = "Marker", p = "p",
          logp = T, 
          #col = c("red", "blue"),
          genomewideline = F, suggestiveline = F, 
          annotateTop = T,
          highlight = vcmax_7_snps,
          ylim=c(0,4), main = "Vcmax")

jmax <-  tassel %>% filter(.$Trait == "Jmax")
jmax <- jmax[-1,]
jmax <- arrange(jmax, p)

jmax_7_snps <- jmax[1:7,2]

manhattan(jmax, chr = "Chr", bp = "Pos", snp = "Marker", p = "p",
          logp = T, 
          #col = c("red", "blue"),
          genomewideline = F, suggestiveline = F, 
          annotateTop = T,
          highlight = jmax_7_snps,
          ylim=c(0,4), main = "Jmax")
