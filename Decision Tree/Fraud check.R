setwd("E:/R_basics/ds")
Fraud_Analytics <- read.csv("Fraud_Analytics.csv", sep = "," , header = TRUE)
head(Fraud_Analytics)
attach(Fraud_Analytics)
names(Fraud_Analytics)
summary(Fraud_Analytics)
category <- ifelse(Taxable.Income <= 30000, "Risky" , "Good")
customer <- data.frame(Fraud_Analytics,category)
View(customer)
head(customer)
table(customer$category)
customer <- customer[c(-3)]
names(customer)
Undergrad_1 <- factor(Undergrad,labels = c(0,1))
Marital.Status_1 <- factor(Marital.Status,labels = c(0,1,3))
Urban_1 <- factor(Urban,labels = c(0,1))
category_1 <- factor(category,labels = c(0,1))
customer_1 <- data.frame(Undergrad_1,Marital.Status_1,City.Population,Work.Experience,Urban_1,category_1)
set.seed(2) #split data into test and train datasets, set.seed signifies that each time you take same sample of data
train <- sample(1:nrow(customer_1),nrow(customer_1)/2)
length(train)
test <- -train
training_data <- customer_1[train,]
test_data <- customer_1[test,]
names(test_data)
testing_high <- category_1[test] #Catrgory of testing data set is assigned to testing_high variable
length(testing_high)
summary(training_data) #Desprictive Analytics
library(tree)
library(cluster)
na.omit(training_data)
library(dplyr)
tree_model <- tree(training_data$category_1 ~. , training_data) #Selecting y till x
plot(tree_model)
text(tree_model,pretty = 0)
Decision_tree_prediction <- predict(tree_model,test_data, type = 'class')
mean(Decision_tree_prediction != testing_high)
set.seed(3)
cross_val_tree <- cv.tree(tree_model, FUN = prune.misclass)
names(cross_val_tree)
summary(cross_val_tree)
plot(cross_val_tree$size,cross_val_tree$dev, type = 'b')
Decision_tree_p_model <- prune.misclass(tree_model, k = NULL, best = 4, newdata = training_data,nwts = 1 , loss = 1, eps = 1e-3)
plot(Decision_tree_p_model) 
text(Decision_tree_p_model, pretty = 0)
Decision_tree_prediction <- predict(Decision_tree_p_model,test_data, type = 'class')
summary(Decision_tree_prediction)
mean(Decision_tree_prediction != testing_high)
