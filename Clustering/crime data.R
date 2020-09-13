data("USArrests")  # Load the data set
head(USArrests)
str(USArrests)
summary(USArrests)
any(is.na(USArrests))
library(corrplot)
corrplot(cor(USArrests), method = "number",
         type = "lower")
USArrests <- scale(USArrests)
dim(USArrests)
head(USArrests,n=5)
library(cluster)
library(factoextra)
set.seed(123)
crime <- sample(1:50,10)

crime_1 <- USArrests[crime,]
head(crime_1)
dist.eucl <- dist(crime_1, method = "euclidean")
head(dist.eucl)
round(as.matrix(dist.eucl)[1:4, 1:4], 1)
fviz_dist(dist.eucl)
wss <- sapply(1:crime, 
              function(k){kmeans(USArrests, k, nstart=20,iter.max = 15                     )$tot.withinss})
plot(1:crime, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
fviz_nbclust(USArrests, kmeans, method = 'wss') +
  geom_vline(xintercept = 4, linetype=5, col= "darkred")
km.res <- kmeans(USArrests, 4, nstart = 20)
km.res
km.res$betweenss
139.5968/196
df_member <- cbind(USArrests, cluster = km.res$cluster)
head(df_member,10)
fviz_cluster(km.res, data = USArrests,
             palette=c("red", "blue", "black", "darkgreen"),
             ellipse.type = "euclid",
             star.plot = T,
             repel = T,
             ggtheme = theme())
