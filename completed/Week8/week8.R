
# data <- cbind(kValues, AccBC.all, AccTC.all)
# data
# data <- data.frame(data)
# 
# data <- data.frame(cbind(kValues, AccBC.all, AccTC.all))

kValues <- c(3, 5, 10, 15, 20, 25)
# kValues <- sprintf("%02d", c(3, 5, 10, 15, 20, 25))

AccBC.all <- c(AccBC.3, AccBC.5, AccBC.10, AccBC.15, AccBC.20, AccBC.25) * 100
label <- c("Binary")
pctAcc <- round(AccBC.all, digits = 2)
# BCAccSet <- data.frame(cbind(label, kValues, pctAcc))
BCAccSet <- cbind(label, kValues, pctAcc)


AccTC.all <- c(AccTC.3, AccTC.5, AccTC.10, AccTC.15, AccTC.20, AccTC.25) * 100
label <- c("Trinary")
pctAcc <- round(AccTC.all, digits = 2)
# TCAccSet <- data.frame(cbind(label, kValues, pctAcc))
TCAccSet <- cbind(label, kValues, pctAcc)

# dataSet <- rbind(BCAccSet, TCAccSet)
dataSet <- data.frame(rbind(BCAccSet, TCAccSet))

# dataSet$label <- factor(dataSet$label)
dataSet$kValues <- as.numeric(dataSet$kValues)
dataSet$pctAcc <- as.numeric(dataSet$pctAcc)

ggplot(data = dataSet, mapping = aes(kValues, pctAcc, color = label)) + geom_point()

# ggplot(data = kAccData, mapping = aes(kValues, c(AccBC.all, AccTC.all))) + geom_point()
# 
# ggplot(kAccData) + geom_point(aes(kValues, AccBC.all)) + geom_point(aes(kValues, AccTC.all))



kValues <- c(3, 5, 10, 15, 20, 25)

pctAcc <- c(AccBC.3, AccBC.5, AccBC.10, AccBC.15, AccBC.20, AccBC.25) * 100
label <- c("Binary")
BCAccSet <- data.frame(label, kValues, pctAcc)

pctAcc <- c(AccTC.3, AccTC.5, AccTC.10, AccTC.15, AccTC.20, AccTC.25) * 100
label <- c("Trinary")
TCAccSet <- data.frame(label, kValues, pctAcc)

dataSet <- rbind(BCAccSet, TCAccSet)

ggplot(data = dataSet, mapping = aes(kValues, pctAcc, color = label)) + geom_point()


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



#generate some data
traindata<-matrix(rnorm(400),ncol=2)
traindata=scale(traindata,center = T,scale=T) # Feature Scaling
#get the full kmeans
km.cluster = kmeans(traindata, 2,iter.max=20,nstart=25)
#define a (euclidean) distance function between two matrices with two columns
myDist<-function(p1,p2) sqrt((p1[,1]-p2[,1])^2+(p1[,2]-p2[,2])^2)
#gets the distances
myDist(traindata[km.cluster$cluster==1,],km.cluster$centers[1,,drop=FALSE])
myDist(traindata[km.cluster$cluster==2,],km.cluster$centers[2,,drop=FALSE])

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


testa <- c(1, 2, 3, 4, 5)
testb <- c(2, 3, 4, 5, 6)
testc <- c("A", "B", "C", "D", "E")

testdf <- data.frame(testa, testb, testc, row.names = "Column A", "Column B", "Column C")
