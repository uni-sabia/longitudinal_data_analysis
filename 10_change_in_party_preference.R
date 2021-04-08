# Do people who vote for CDU change their political preference over time? 
library(TraMineR)
library(reshape2)
library(cluster)
library(WeightedCluster)
library(descr)
library(haven)
library(ggplot2)
library(descr)

# Load SOEP data
## Data is already in wide format
cdu <- read_dta("data/08_soep/CDU.dta")

# Define the sequence object
seq1 <- seqdef(cdu[,2:12])

# Legend
cpal(seq1) <- c("black", "green", "gray", "blue", "red")
seqlegend(seq1, bty="n")

# Draw sequence index plot
seqIplot(seq1, xaxis=FALSE, with.legend=FALSE,)
axis(1, at=c(0,11), labels=c("20","30"))

# Distribution plot
seqdplot(seq1, xaxis=FALSE, with.legend=FALSE)
axis(1, at=c(0,11), labels=c("20","30"))

# Distance
distance <- seqdist(seq1, method="LCS")
cluster <- hclust(as.dist(distance), method="ward.D")
plot(cluster)

# Clusters
wardrange <- as.clustrange(cluster, diss=distance, ncluster=10)
plot(wardrange, stat=c("ASW", "ASWw"))
print(wardrange) # Pick the number of clusters whose ASW is the highest

# Typology
labels <- c("Always CDU", "Switchers")
tree <- cutree(cluster, k=2)
tree1 <- factor(tree, levels=1:2, labels=labels)
seqIplot(seq1, with.legend="right", group=tree1)

# Who is in which cluster?
## Gender
cdu$clusters <- tree1
crosstab(cdu$clusters, cdu$GENDER, prop.c=T)

table1 <- table(cdu$clusters, cdu$GENDER)
print(table1)
table2 <- prop.table(table1, 2)
table3 <- as.data.frame(table2)
print(table2)

ggplot(table3, aes(fill=Var1, x=Var2, y=Freq)) +
  geom_bar(stat="identity") +
  ylim(0,1) +
  ggtitle("") +
  ylab("%") +
  xlab("")

## Education
cdu$clusters <- tree1
crosstab(cdu$clusters, cdu$EDU, prop.c=T)

table1 <- table(cdu$clusters, cdu$EDU)
print(table1)
table2 <- prop.table(table1, 2)
table3 <- as.data.frame(table2)
print(table2)

ggplot(table3, aes(fill=Var1, x=Var2, y=Freq)) +
  geom_bar(stat="identity") +
  ylim(0,1) +
  ggtitle("") +
  ylab("%") +
  xlab("")
