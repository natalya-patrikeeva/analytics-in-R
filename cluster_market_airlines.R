# Market segmentation for airlines.
rm(list=ls())
airlines = read.csv("AirlinesCluster.csv")
summary(airlines)
str(airlines)

install.packages("caret")
library(caret)

# Normalizing the Data: mean zero, std 1
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)

?preProcess
summary(airlinesNorm)
str(airlinesNorm)
sd(airlinesNorm$QualMiles)

# Hierarchical Clustering
distance = dist(airlinesNorm, method = "euclidean")
clusters = hclust(distance, method = "ward.D")
plot(clusters)

# Divide the data points into 5 clusters
rect.hclust(clusters, k=5, border = "red")
HierClusters = cutree(clusters, k = 5)
table(HierClusters)

# compare the average values in each of the variables for the 5 clusters (the centroids of the clusters).
tapply(airlines$Balance, HierClusters, mean)
tapply(airlines$QualMiles, HierClusters, mean)
tapply(airlines$BonusMiles, HierClusters, mean)
tapply(airlines$BonusTrans, HierClusters, mean)
tapply(airlines$FlightMiles, HierClusters, mean)
tapply(airlines$FlightTrans, HierClusters, mean)
tapply(airlines$DaysSinceEnroll, HierClusters, mean)

# alternatively
colMeans(subset(airlines, HierClusters == 1))
colMeans(subset(airlines, HierClusters == 2))
colMeans(subset(airlines, HierClusters == 3))
colMeans(subset(airlines, HierClusters == 4))
colMeans(subset(airlines, HierClusters == 5))

# even shorter.
# Efficient approach.
spl = split(airlines,HierClusters)
#spl[[1]] # cluster 1
lapply(split(airlines, HierClusters), colMeans)

# K-Means Clustering
k = 5
set.seed(88)
KMC = kmeans(airlinesNorm, centers = k, iter.max = 1000)
str(KMC)
table(KMC$cluster)
KMC$centers
colMeans(subset(airlines, KMC$cluster == 1))
