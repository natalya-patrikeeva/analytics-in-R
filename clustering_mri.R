rm(list=ls())
healthy = read.csv("healthy.csv", header = FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)
image(healthyMatrix, axes = FALSE, col=grey(seq(0,1,length=256)))
healthyVector = as.vector(healthyMatrix)

str(healthyVector)
# distance matrix size: n*(n-1)/2
n=365636
n*(n-1)/2 # 67 billion values to store in a matrix
# distance matrix is going to take a long time to compute!
# distance = dist(healthyVector, method = "euclidean")

# K-means clustering
k=5
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)
healthyClusters = KMC$cluster
KMC$centers
KMC$centers[2]

dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyClusters, axes = FALSE, col = rainbow(k))

# scree plot has the number of clusters on the x-axis, and the sum of the 
# within-cluster sum of squares on the y-axis. want very small within-cluster sum of squares, 
# so that the points are all very close to their centroid. 

# run the k-means algorithm with two clusters
KMC2 = kmeans(healthyVector, centers = 2, iter.max = 1000)
KMC2$withinss # within-cluster sum of squares for each cluster

KMC3 = kmeans(healthyVector, centers = 3, iter.max = 1000)
KMC4 = kmeans(healthyVector, centers = 4, iter.max = 1000)
KMC5 = kmeans(healthyVector, centers = 5, iter.max = 1000)
KMC6 = kmeans(healthyVector, centers = 6, iter.max = 1000)
KMC7 = kmeans(healthyVector, centers = 7, iter.max = 1000)
KMC8 = kmeans(healthyVector, centers = 8, iter.max = 1000)
KMC9 = kmeans(healthyVector, centers = 9, iter.max = 1000)
KMC10 = kmeans(healthyVector, centers = 10, iter.max = 1000)

NumClusters = seq(2,10,1)
SumWithinss = c(sum(KMC2$withinss), sum(KMC3$withinss),sum(KMC4$withinss), sum(KMC5$withinss), sum(KMC6$withinss), sum(KMC7$withinss), sum(KMC8$withinss), sum(KMC9$withinss),sum(KMC10$withinss))
SumWithinss
plot(NumClusters, SumWithinss, type="b")

# to determine the best number of clusters, look for elbow in the plot above. 
# find the number of clusters for which increasing the number of clusters further does not
# significantly help to reduce the within-cluster sum of squares.
# Beyond 5, increasing the number of clusters doesn't really reduce the within-cluster sum of squares too much.

# More efficient way.
SumWithinss2 = sapply(2:10, function(x) sum(kmeans(healthyVector, centers = x, iter.max = 1000)$withinss))
SumWithinss2


# Detecting tumors
tumor = read.csv("tumor.csv", header=FALSE)
tumorMatrix = as.matrix(tumor)
str(tumorMatrix)
tumorVector = as.vector(tumorMatrix)

install.packages("flexclust")
library(flexclust)

# object class KCCA
KMC.kcca = as.kcca(KMC, healthyVector)

# cluster the pixels in the tumorVector using the predict function.
tumorClusters = predict(KMC.kcca, newdata = tumorVector)
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes=FALSE, col=rainbow(k))
dim(tumorClusters)
