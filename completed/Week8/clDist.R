setwd("C:/Users/micha/OneDrive/Documents/GitHub/DSC520/")
cluster_DF <- read_csv("completed/Week8/data/clustering-data.csv")


clusters_2 <- kmeans(cluster_DF, 2)
clusterDF$k2 <- as.factor(clusters.2$cluster)

ggplot(clusterDF, mapping = aes(x, y, color = k2)) + geom_point()


## ELBOW METHOD
set.seed(42)
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(clusterDF, k, nstart = 10 )$tot.withinss
}
# Compute and plot wss for k = 1 to k = 15
k.values <- 2:12
# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


# #generate some data
# traindata<-matrix(rnorm(400),ncol=2)
# traindata=scale(traindata,center = T,scale=T) # Feature Scaling
# #get the full kmeans
# km.cluster = kmeans(cluster_DF, 2,iter.max=20,nstart=25)
#define a (euclidean) distance function between two matrices with two columns
myDist<-function(p1,p2) sqrt((p1[,1]-p2[,1])^2+(p1[,2]-p2[,2])^2)
#gets the distances
dist2.1 <- myDist(cluster_DF[clusters_2$cluster==1,],clusters_2$centers[1,,drop=FALSE])
dist2.2 <- myDist(cluster_DF[clusters_2$cluster==2,],clusters_2$centers[2,,drop=FALSE])

clusters.2$cluster==1
p1 <- clusterDF[clusters.2$cluster==1,1:2]
clusterDF[clusters.2$cluster==2,1:2]
p2 <- clusters.2$centers[1,,drop = FALSE]
clusters.2$centers[2,,drop = FALSE]
clusters.2$centers[1,]
sqrt((p1[,1]-p2[,1])^2+(p1[,2]-p2[,2])^2)
dist <- sqrt((p1[,1]-p2[,1])^2+(p1[,2]-p2[,2])^2)
mean(dist[,1])
mean(dist$x)

mean(sqrt((p1[,1]-p2[,1])^2+(p1[,2]-p2[,2])^2)[,1])

clusters.10$centers[1:10, 1:2]


clusters_2


# Get distances from each cluster
dist.1 <- myDist(clusterDF[clusters.6$cluster==1,],clusters.6$centers[1,,drop=FALSE])
dist.2 <- myDist(clusterDF[clusters.6$cluster==2,],clusters.6$centers[2,,drop=FALSE])
dist.3 <- myDist(clusterDF[clusters.6$cluster==3,],clusters.6$centers[3,,drop=FALSE])
dist.4 <- myDist(clusterDF[clusters.6$cluster==4,],clusters.6$centers[4,,drop=FALSE])
dist.5 <- myDist(clusterDF[clusters.6$cluster==5,],clusters.6$centers[5,,drop=FALSE])
dist.6 <- myDist(clusterDF[clusters.6$cluster==6,],clusters.6$centers[6,,drop=FALSE])
# Get average distance
k6.avdist <- sum(dist.1, dist.2, dist.3, dist.4, dist.5, dist.6)/nrow(clusterDF)
k6.avdist

cl <- clusters.7
kDist <- function(cl, nItter=7) {
  for(i in 1:nItter) {
    dist.[[i]] <- myDist(clusterDF[cl$cluster==[[i]],],cl$centers[ [[i]],,drop=FALSE])
  }
}

avDist <- c(k2.avdist, k3.avdist, k4.avdist, k5.avdist, k6.avdist, k7.avdist, k8.avdist, k9.avdist, k10.avdist, k11.avdist, k12.avdist)
avDist
k.values

k.dist <- data.frame(k.values, avDist)
ggplot(k.dist, mapping = aes(k.values, avDist)) + geom_line() + geom_point()
