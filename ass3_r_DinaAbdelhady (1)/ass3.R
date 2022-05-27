library(dplyr)
library(ggplot2)
library(cluster) 

# Read the Data
data <- read.csv("framingham.csv",header=TRUE);
data$TenYearCHD <- factor(data$TenYearCHD)

ggplot(data, aes(male, age, color = TenYearCHD)) + geom_point()

#standardize Age
data$age <-scale(data$age,center=TRUE,scale=TRUE)
#kmeans k = 4 clusters

#K-Means Clustering Algorithm
set.seed(917)

#Run k-means cluster of the dataset
Cluster_kmean <- kmeans(data[,1:2], 4, nstart = 20)

#Tabulate the cross distribution
table(Cluster_kmean$cluster,data$TenYearCHD)

str(data)

Cluster_kmean$cluster <- factor(Cluster_kmean$cluster)
Cluster_kmean$cluster
data[1:2] <- lapply(data[1:2], as.numeric)
str(data)
ggplot(data, aes(male , age, color = TenYearCHD)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = Cluster_kmean$cluster) + 
  scale_color_manual(values = c('black', 'red', 'green','blue'))


# Elbow Curve

wss <- (nrow(data)-1)*sum(apply(data[,1:2],2,var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(data[,1:2],centers=i)$withinss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")



library(cluster) 
silhouette_score <- function(k){
  km <- kmeans(data[,1:2], k, nstart = 20)
  ss <- silhouette(km$cluster, dist(data[,1:2]))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

#Run k-means cluster of the dataset
Cluster_kmean <- kmeans(data[,1:2], 6, nstart = 20)

#Tabulate the cross distribution
table(Cluster_kmean$cluster,data$TenYearCHD)

str(data)

Cluster_kmean$cluster <- factor(Cluster_kmean$cluster)
Cluster_kmean$cluster
data[1:2] <- lapply(data[1:2], as.numeric)
str(data)
ggplot(data, aes(male , age, color = TenYearCHD)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = Cluster_kmean$cluster) + 
  scale_color_manual(values = c('black', 'red', 'green','blue'))
