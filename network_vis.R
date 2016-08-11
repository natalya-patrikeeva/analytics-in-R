# Visualizing network data from facebook
rm(list=ls())
edges = read.csv("edges.csv")
str(edges)
summary(edges)
head(edges)
users = read.csv("users.csv")
str(users)
summary(users)
head(users)
table(users$locale)
146*2/59
mean(table(edges$V1))
mean(table(edges$V2))
table(users$gender, users$school) 

install.packages("igraph")
library(igraph)
?graph.data.frame

g = graph.data.frame(edges, FALSE, users)
g
plot(g,vertex.size=5, vertex.label=NA)
sort(degree(g))
table(degree(g) >= 10)
V(g)$size = degree(g)/2 + 2
plot(g, vertex.label=NA)
V(g)$size
max(V(g)$size)
min(V(g)$size)

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)
table(V(g)$color,V(g)$size)

summary(users)
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "blue"
V(g)$color[V(g)$school == "AB"] = "red"
plot(g, vertex.label=NA)
table(V(g)$color,V(g)$school)

summary(users)
colors()
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "skyblue"
V(g)$color[V(g)$locale == "B"] = "salmon"
plot(g, vertex.label=NA)
table(V(g)$color,V(g)$locale)

?igraph.plotting
plot(g, vertex.label=NA, edge.width=3)

install.packages("rgl")
library(rgl)
rglplot(g, vertex.label=NA)
g
