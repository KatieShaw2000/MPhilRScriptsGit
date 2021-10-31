#Packages needed

library(readxl)
library(ggplot2)

#Get data

SLA <- read_excel("~/OneDrive - University of Cambridge/MPhil/Data2021/SLA.xlsx")
SLA = data.frame(SLA)

SLA$Rep <- as.factor(SLA$Rep)

#Look at area

AreaGraph <- ggplot(SLA, aes(x=SLA, y=Area)) + geom_point()
AreaGraph

#Look at mass

MassGraph <- ggplot(SLA, aes(x=SLA, y=Mass)) + geom_point()
MassGraph

#Plot genotype-by-genotype basis with both replicates

SLABasicGraph <- ggplot(SLA, aes(x=Name, y=SLA, color=Rep)) + geom_point(size=1) +
  ggtitle("SLA vs Genotype")

SLABasicGraph

#Plot SLA against heading date 

SLADateGraph <- ggplot(SLA, aes(x=Heading_date, y=SLA, color=Rep)) + geom_point(size=1, shape=4) +
  ggtitle("SLA vs Heading Date")

SLADateGraph

#Plot SLA from first vs second rep means against each other --> should see positive correlation if same in different replicate

rep1 <- SLA[which(SLA$Rep=='1'),]
rep2 <- SLA[which(SLA$Rep=='2'),]

PosCorGraph <- ggplot(rep1, aes(x=SLA, y=rep2$SLA)) +geom_point()

PosCorGraph #Need to get means of each genotype for this to work --> ask John?

  
