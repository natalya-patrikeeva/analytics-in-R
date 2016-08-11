# Document clustering with Daily kos
rm(list=ls())
docs = read.csv("dailykos.csv")
str(docs)
dim(docs)

distances = dist(docs, method="euclidean")
max(distances)
clusters = hclust(distances, method = "ward.D")

# plot the dendrogram of the clustering algorithm
plot(clusters)

# label each data point with its cluster. We selected 7 clusters.
clusterGroups = cutree(clusters, k = 7)

# Efficient approach.
spl = split(docs,clusterGroups)
spl[[1]] # cluster 1
which.max(str(spl))

# compute the mean frequency values of each word in cluster 1, output top 6. Each column is a word. 
tail(sort(colMeans(spl[[1]])))
tail(sort(colMeans(spl[[2]])))
tail(sort(colMeans(spl[[3]])))
tail(sort(colMeans(spl[[4]])))
tail(sort(colMeans(spl[[5]])))
tail(sort(colMeans(spl[[6]])))
tail(sort(colMeans(spl[[7]])))
?tail
?table

# K-Means clustering
k=7
set.seed(1000)
KMC = kmeans(docs, centers = k)
str(KMC)

cluster3 = subset(docs, KMC$cluster == 3)
KMCluster = split(docs, KMC$cluster)
str(KMCluster)
str(KMCluster[[3]])
tail(sort(colMeans(KMCluster[[1]])))
tail(sort(colMeans(KMCluster[[2]])))
tail(sort(colMeans(KMCluster[[3]])))
tail(sort(colMeans(KMCluster[[4]])))
tail(sort(colMeans(KMCluster[[5]])))
tail(sort(colMeans(KMCluster[[6]])))
tail(sort(colMeans(KMCluster[[7]])))

# compare the cluster assignment of hierarchical clustering to the cluster assignment of k-means clustering.
str(clusterGroups)
str(KMC)
table(clusterGroups, KMC$cluster)
