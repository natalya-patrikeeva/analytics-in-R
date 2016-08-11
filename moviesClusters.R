# Cluster movies by genre using hierarchical clustering.

rm(list=ls())
movies = read.table("movieLens.txt", header=FALSE, sep="|", quote="\"")
str(movies)
colnames(movies) = c("ID","Title","ReleaseDate","VideoReleaseDate", "IMDB", "Unknown","Action","Adventure", "Animation","Childrens","Comedy","Crime","Documentary","Drama","Fantasy","FilmNoir","Horror","Musical","Mystery","Romance","SciFi","Thriller","War","Western")
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL
movies = unique(movies)
str(movies)
dim(movies)
movies[1]
movies[2]
dim(movies[1])

table(movies$Comedy)
table(movies$Western)
table(movies$Romance == 1 & movies$Drama == 1)

# First, compute distances between all data points.
# Cluster using genre variables - columns 2-20.
distances = dist(movies[2:20], method="euclidean")

# Second, cluster the points.
clusterMovies = hclust(distances, method = "ward.D")

# plot the dendrogram of the clustering algorithm
plot(clusterMovies)

# label each data point with its cluster. We selected 10 clusters.
clusterGroups = cutree(clusterMovies, k = 10)

# compute percentage of movies in each genre and cluster.
# cutree functions divides data points into 10 clusters.
# tapply function computes the mean value of the action variable for each cluster (0 or 1 for each movie).
# so the mean value is the percentage of movies that belong to Action genre.
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

# Using tapply function for each variable is too tedius.
# Instead, to find the cluster centroids:
colMeans(subset(movies[2:20]), clusterGroups == 1)
# repeat for each cluster

# Efficient approach.
spl = split(movies[2:20],clusterGroups)
spl[[1]] # is the same as subset(movies[2:20], clusterGroups == 1) - which movies belong to cluster 1
# Split data into subsets based on clusters.
colMeans(spl[[1]]) # compute centroid of cluster 1

# output the cluster centroids for all clusters.
# lapply function runs the second arg (colMeans) on each element of the first argument (each cluster subset in spl).
lapply(spl, colMeans)

# Recommendation system
subset(movies, Title == "Men in Black (1997)")
clusterGroups[257]

cluster2 = subset(movies, clusterGroups == 2)
cluster2$Title[1:10]


# Cluster with 2 clusters
clusterGroupsTwo = cutree(clusterMovies, k = 2)
spltwo = split(movies[2:20],clusterGroupsTwo)
lapply(spltwo, colMeans)
