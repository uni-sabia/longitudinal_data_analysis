# Sequence analysis using Tramine R

library(TraMineR)
library(reshape2)
library(cluster)
library(WeightedCluster)
library(descr)
library(haven)
library(ggplot2)

# What did you do yesterday? 

ID <-as.numeric(c(1,1,1,1,1,1,1,1,1,1,1,1,1))
TIME <-as.numeric(c(8,9,10,11,12,13,14,15,16,17,18,19,20))
ACTIVITY<-
  as.factor(c("WORK","WORK","LECTURE","LECTURE","LECTURE","EAT","WORK","WORK","SPORT", "SPORT", "LEISURE","LEISURE","LEISURE"))
DATA01 <- data.frame(ID,TIME,ACTIVITY) 

# Turn data into wider format
DATA02 <- reshape(DATA01, idvar ="ID", timevar="TIME", direction="wide")

# Define and draw sequence index plot
DATA03 <- seqdef(DATA02[,2:13])
seqIplot(DATA03)

# Change colors
seqstat1(DATA03[, 1:12])
cpal(DATA03) <- c("blue", "black", "red", "red4")
axis(1, at=c(0,12), labels=c("8am, 8pm"))

# Legend
seqlegend(DATA03, bty="n")

# Put pieces together
par(mfrow=c(2,1))
cpal(DATA03) <- c("yellow", "green", "red", "blue")
seqIplot(DATA03, xaxis=FALSE, with.legend=FALSE, )
axis(1, at=c(0,12), labels=c("8am", "8pm"))
seqlegend(DATA03, bty="n")

# Change in party preference (Germany)
#Insert Data
ID <-as.numeric(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5,6,6,6,6,6))
TIME01 <-as.numeric(c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5))
TIME<-as.factor(c("CDU","SPD","GREEN","SPD","SPD","CDU","SPD","GREEN","GREEN", "GREEN",
                  "CDU","SPD","GREEN", "SPD","GREEN","CDU","SPD","SPD","SPD","SPD","SPD","SPD","GREEN","GREEN",
                  "SPD", "CDU","SPD","GREEN", "SPD","SPD"))
DATA01 = data.frame(ID,TIME01,TIME)
DATA02<-reshape(DATA01,idvar="ID",timevar="TIME01",direction="wide")

# Declare the data as a sequence file
DATA03 <- seqdef(DATA02[,2:6])

# Plot the sequence index
cpal(DATA03) <- c("black", "green","red")
seqIplot(DATA03, border=T, with.legend="right")

# Plot the state distribution
seqdplot(DATA03, border = T, with.legend="right")

# Generate and display the distance matrix
DISTANCE <- seqdist(DATA03, method="LCS")
print(DISTANCE)

# Cluster the data
CLUSTER <- hclust(as.dist(DISTANCE), method="ward.D")
plot(CLUSTER)

# Group the data into 2 clusters
TREE <- cutree(CLUSTER, k=2) # k: how many groups do you want? 
print(TREE)

# Display the sequence index plots of the two clusters 
seqIplot(DATA03, border=T, with.legend="right", group=TREE)

# Label the two clusters in an appropriate matter
LABELS <- c("SPD changers", "Green Transitioners")
TREE01 <- factor(TREE, levels=1:2, labels=LABELS)
seqIplot(DATA03, border=T, with.legend="right", group=TREE01)
