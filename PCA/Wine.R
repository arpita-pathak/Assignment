Wine <- read.csv(file.choose()) 
head(Wine)
str(Wine)
summary(Wine)
any(is.na(Wine))
Wine_scale <- scale(Wine[-1]) 
dim(Wine_scale)
head(Wine_scale,3)
library(cluster)
library(factoextra)
library(ggplot2)
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
wssplot(Wine_scale, nc=10)
fviz_nbclust(Wine_scale, kmeans, method = 'wss') +
  geom_vline(xintercept =3, linetype=5, col= "darkred")
k.means <- kmeans(Wine_scale, 3,nstart = 25) # k = 4
k.means
# Centroids:
k.means$centers

# Cluster size:
k.means$size

clusplot(Wine_scale, k.means$cluster, main='2D representation of the Cluster',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)


fviz_cluster(object = k.means, # kmeans object 
             data = Wine_scale, # data used for clustering
             ellipse.type = "norm",
             geom = "point",
             palette = "jco",
             main = "",
             ggtheme = theme_minimal())
