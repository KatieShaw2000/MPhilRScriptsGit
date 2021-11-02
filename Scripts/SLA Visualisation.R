#Packages needed

library(readxl)
library(ggplot2)

#Set working directory

setwd("~/OneDrive - University of Cambridge/MPhil/Phenotyping Campaign")

#Get data

SLA <- read_excel("SLA Data/SLA.xlsx")
SLA = data.frame(SLA)

SLA$Rep <- as.factor(SLA$Rep)

#Look at area

AreaGraph <- ggplot(SLA, aes(x=SLA, y=Area)) + geom_point() + ggtitle("Leaf Area vs SLA")
AreaGraph

#Look at mass

MassGraph <- ggplot(SLA, aes(x=SLA, y=Mass)) + geom_point() + ggtitle("Leaf mass vs SLA")
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

#Get means for each genotype for each rep 1 or 2

rep1means <- length(vector())
  
for (value in unique(rep1$Name)){
  subset <- subset(rep1, rep1$Name == value)
  mean <- mean(subset$SLA, na.rm=TRUE)
  rep1means <- append(rep1means, mean)
}

rep2means <- length(vector())

for (value in unique(rep2$Name)){
  subset <- subset(rep2, rep2$Name == value)
  mean <- mean(subset$SLA, na.rm=TRUE)
  rep2means <- append(rep2means, mean)
}

rep1means <- rep1means[-1]
rep2means <- rep2means[-1]

plot(x=rep1means, y=rep2means, main = paste("SLA rep 2 means vs SLA rep 1 means"), 
     xlab = "Rep1 SLA means", ylab = "Rep2 SLA means", xlim = c(100,350), ylim = c(100,350))
  