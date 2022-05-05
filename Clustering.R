#library
#install.packages("tidyverse")
#install.packages("gplots")
#install.packages("cluster")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(gplots)
library(cluster)

# Read the Data
Data <- read.csv("Shopping_Customers.csv",header=TRUE)
str(Data)
glimpse(Data)

#remove column Gender and CustomerID
#Data$CustomerID<- NULL
#Data$Gender<- NULL
#str(Data)
#glimpse(Data)

#other remove column Gender and CustomerID 
Data1=Data[,3:5]
str(Data1)
glimpse(Data1)

#Apply K-Means Clustering Algorithm on the data set
set.seed(100)
Cluster_kmean <- kmeans(Data1, 2, nstart = 20)

#Tabulate the cross distribution
table(Cluster_kmean$cluster)

#save cluster in Dataframe
cluster_df<-data.frame(Cluster_kmean$cluster)

#plot k-means clusters
clusplot(Data[,4:5],cluster_df[,1],main = 'Cusplot')

#plot k-means clusters2
Cluster_kmean$cluster <- factor(Cluster_kmean$cluster)
ggplot(Data1, aes(Annual.Income..k.., Spending.Score..1.100.)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = Cluster_kmean$cluster) + 
  scale_color_manual(values = c('black', 'red'))

#heat map
x <- Data1
x <- as.matrix(x)
heatmap(x)

#other heat map in save in pdf
pdf('a.pdf',width = 8.267, height = 11.692)
heatmap.2(x, scale = "none",col = bluered(100),
          trace = "none", density.inf="none")
dev.off()

# Elbow Curve
wss <- (nrow(Data1)-1)*sum(apply(Data1,2,var))
for (i in 2:10) {
  wss[i] <- sum(kmeans(Data1,centers=i)$withinss)
}
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#silhouette function 
library(cluster)    
avg_sil <- function(k) {
  km.res <- kmeans(Data1, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(Data1))
 mean(ss[, 3])
}

# Compute and plot Average silhouette for k = 2 to k = 15
k.values <- 2:15
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

#apply the hierarchical clustering algorithm and plot dendrogram(complete linkage)
clusters <- hclust(dist(Data1))
plot(clusters,hang = -1,labels = FALSE,main=" complete linkage")

#apply the hierarchical clustering algorithm and plot dendrogram (single)
clusters1 <- hclust(dist(Data1),method = "single")
plot(clusters1,hang = -1,labels = FALSE,main = "single")
