## Libraries
library("dummies")
library("dendextend")
library("dendextendRcpp")
library("gridExtra")
library("cluster")
library("factoextra")
library("MASS")
library("fpc")

## Set the working directory to where the Excel file is
setwd('J:\\ISB Business Analytics\\Data Mining\\Data Mining Assignment 1')

## Input file read
input     <- read.csv("EastWestAirlinesClusterCSV.csv",header=TRUE)

## 1. Loading and preparing data
mydatawd  <- input[,2:11]

#Creating Dummy variables for categorical data
mydata    <- dummy.data.frame(mydatawd, names = "cc1_miles", omit.constants=FALSE )
mydata    <- dummy.data.frame(mydatawd, names = "cc2_miles", omit.constants=FALSE )
mydata    <- dummy.data.frame(mydatawd, names = "cc3_miles", omit.constants=FALSE )

# Standardize Data
my_data   <- scale(mydata)

# 2. Compute dissimilarity matrix
d         <- dist(my_data, method = "euclidean")

# Hierarchical clustering using Ward's method
res.hc    <- hclust(d, method = "ward.D2" )

# Hierarchical clustering using Ward's method

Dend1   <- as.dendrogram(res.hc)

#Random Sample1 with 95% of data
input2=input[sample(nrow(my_data),replace=F,size=0.95*nrow(input)),]

d       <- dist(input2, method = "euclidean")
res2.hc <- hclust(d, method = "ward.D2" )
Dend2   <- as.dendrogram(res2.hc)

#Random Sample2 with 95% of data
input3=input[sample(nrow(my_data),replace=F,size=0.95*nrow(input)),]

d       <- dist(input3, method = "euclidean")
res3.hc <- hclust(d, method = "ward.D2" )
Dend3   <- as.dendrogram(res3.hc)
# Global Comparison of Dendograms
# Total Population Metrics
Dend1

# Random Sample 1 Metrics
Dend2

# Random Sample 2 Metrics
Dend3

# Comparison of Population, Random sample 1 and Random sample 2 dendogarms.
all.equal(Dend3, Dend2, Dend1, use.edge.length = TRUE)

## K-means clustering
set.seed(123)
fit         <- kmeans(my_data, 3) # 3 cluster solution

#Aggregation of k-means

mydatak     <- data.frame(mydata, fit$cluster) # append cluster membership
temp        <- aggregate(mydatak, by=list(fit$cluster), FUN=mean)

#to find the size of clusters 
ClusterCo   <- aggregate(mydatak, by=list(fit$cluster), FUN=sum) 
#to find the cluster size
d           <- transform(ClusterCo, clusterSize = fit.cluster / Group.1)
d           <- transform(d, fit.cluster= fit.cluster/ clusterSize)
temp$clusterSize   <- d$clusterSize
temp$clusterPCT    <- (d$clusterSize*100)/3999
# transpose to change from horizontal to vertical
temp2       <- t(temp)

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns    <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

temp4       <- round_df(temp2, 2)

#Hierarchical Aggregate calculations

# Hierarchical clustering using Ward's method

#set.seed(123)
groups      <- cutree(res.hc, k=3) # cut tree into 3 clusters
membership  <-as.matrix(groups)
membership  <- data.frame(membership)
names(membership) <- c("cluster")
mydatao     <- data.frame(mydata, membership$cluster) # append cluster membership

temp        <- aggregate(mydatao, by=list(membership$cluster), FUN=mean)

temp2       <- t(temp)

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns    <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

temp5       <- round_df(temp2, 2)

