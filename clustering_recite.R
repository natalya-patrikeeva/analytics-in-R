# Clustering techniques
# For graysclale image, 0 is black, 1 is white.
# For 8 bits/pixel (bpp), 256 color levels (2^8).

# hierarchical clustering
flower = read.csv("flower.csv", header=FALSE)
str(flower)

# need to convert dataset into a matrix
flowerMatrix = as.matrix(flower)
str(flowerMatrix)

# image is 50 x 50 pixels
# need to convert into a 1D vector
flowerVector = as.vector(flowerMatrix)
str(flowerVector)

# distances between every 2 intensity values in an image.
distance = dist(flowerVector, method="euclidean")
str(distance)

# compute hierarchical clusters
# Ward's method is a minimum variance method, which tries to find compact and spherical clusters.
# It's trying to minimize the variance within each cluster and the distane among clusters.
clusterInstensity = hclust(distance, method="ward.D")
plot(clusterInstensity)
# plot boxes around clusters. k=3 specifies 3 cluster split
rect.hclust(clusterInstensity, k=3, border = "red")
flowerClusters = cutree(clusterInstensity, k=3)
flowerClusters

# find mean intensity value of each cluster
tapply(flowerVector, flowerClusters, mean)

# visualize segmented image
dim(flowerClusters) = c(50,50)
image(flowerClusters, axes=FALSE)

# original image
image(flowerMatrix, axes=FALSE, col=grey(seq(0,1,length=256)))
