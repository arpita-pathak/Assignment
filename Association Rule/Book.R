book <- read.csv(file.choose())
## Add new column RFM as the parameter for following data mining process
book$Rcode <- as.character(book$Rcode)
book$Fcode <- as.character(book$Fcode)
book$Mcode <- as.character(book$Mcode)
book$R_F_M <- with(book, paste0(book$Rcode, "_", book$Fcode, "_", book$Mcode))
names(book)
head(book)
set.seed(90)
train.index <- sample(1:nrow(book), 0.6*nrow(book))
train <- book[train.index, ]
valid <- book[-train.index, ]
## Response rate for training data
all_com <- round(table(train$Yes_Florence)/nrow(train), 4)
all_com
## Count each combination's response
z <- table(train$R_F_M, train$Florence)

## Reponse rate for each RFM combination in training data
each_com <- round(prop.table(z, 1), 4)
each_com
## Which combinations have response rates in the training data that are above the overall response?
## Sort RFM combination
sort(each_com[, '1'], decreasing = TRUE)
## Response rate in train data that above average response rate
u <- each_com[each_com[ , '1'] > mean(each_com[ ,'1']), '1']
u
## Reponse rate for each RFM combinations in validation data
z1 <- table(valid$R_F_M, valid$Florence)
each_com1 <- round(prop.table(z1, 1), 4)
## RFM combination in valid data
each_com1[ , '1']
u1 <- each_com1[c(4,5, 6, 12, 14, 17, 18, 19, 21, 22, 23, 24, 31, 33, 38, 39), '1'] ; u1
## : RFM combinations that have response rates that exceed twice the overall response rate
each_com1[each_com1[ , '1'] > (2*all_com[2]), '1']
## RFM combinations that exceed the overall response rate but do not exceed twice that rate
each_com1[each_com1[ , '1'] < (2*all_com[2]) & each_com1[ , '1'] > all_com[2], '1']
## Segment the data
valid$seg <- ifelse(valid$R_F_M == c("1_1_4", "1_2_4", "1_2_5", "1_3_4", "1_3_5", "2_1_2", "2_2_4", "2_3_4", "2_3_5", "3_3_4", "3_3_5", "4_3_5"), "seg2", ifelse(valid$R_F_M == c("1_1_2", "1_3_3", "2_1_1", "2_1_3", "2_3_3", "3_1_1"), "seg1", "seg3"))
yes <- valid$Yes_Florence[valid$Yes_Florence == 1]
breaks <- seq(1, 1600, 200)
yes_cut <- cut(yes, breaks, right=FALSE) 
yes_freq <- table(yes_cut)

cumfreq <- c(0, cumsum(yes_freq))

plot(breaks, cumfreq, main = "Cumulative Chart", 
     xlab = "# of Customer in Validation", 
     ylab ="Cumulative number of buyers")
lines(breaks, cumfreq)
## Slice the dataset
train.df <- train[ ,c(20:22)]
valid.df <- valid[ ,c(20:22)]
## Create dummy variables for training data
library(caret)
dmy <- dummyVars(" ~ .", data = train.df)
train.df1 <- data.frame(predict(dmy, newdata = train.df))
train.df1 <- cbind(train.df1, "FirstPurch" = train$FirstPurch, 
                   "Related.Purchase" = train$Related.Purchase, 
                   "Yes_Florence" = train$Yes_Florence, 
                   "No_Florence" = train$No_Florence)

## Create dummy variable for validation data
dmy1 <- dummyVars(" ~ .", data = valid.df)
valid.df1 <- data.frame(predict(dmy1, newdata = valid.df))
valid.df1 <- cbind(valid.df1, "FirstPurch" = valid$FirstPurch, 
                   "Related.Purchase" = valid$Related.Purchase, 
                   "Yes_Florence" = valid$Yes_Florence, 
                   "No_Florence" = valid$No_Florence)
names(valid.df1)
# Scale and normalize the dataset
library(caret)
train.norm.df <- train.df1
valid.norm.df <- valid.df1
norm.values <- preProcess(train.df1[, c(13,14)], method = c("center","scale"))
train.norm.df[, c(13, 14)] <- predict(norm.values, train.df1[, c(13, 14)])
valid.norm.df[, c(13, 14)] <- predict(norm.values, valid.df1[, c(13, 14)])
## Find out the best k value
library(caret)
library(FNN)
accuracy.df <- data.frame(k = seq(1, 20 ,1), accuracy = rep(0, 20))
for (i in 1:20) {
  knn.pred <- knn(train.norm.df[, c(13, 14)], valid.norm.df[, c(13, 14)], cl = train.norm.df[, 15], k=i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm.df[, 15])$overall[1]
}
accuracy.df
## Demonstrate the confusion matrix in line chart
plot(accuracy.df, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number of neighbors", ylab = "classification accuracy",
     main = "Accuracy and K number of  Neighbors")

## add lines to indicate k with best accuracy
abline(v = which(accuracy.df$accuracy == max(accuracy.df$accuracy)), col = "darkorange", lwd = 1.5)
## add line for max accuracy seen
abline(h = max(accuracy.df$accuracy), col = "grey", lty = 2)
library(FNN)
knn.pred1 <- knn(train.norm.df[, c(13, 14)], valid.norm.df[, c(13, 14)], cl = train.norm.df[, 15], k = 14)
head(book)
full_p <- book[ -c(1,2,4,5,6,23:25)]
full_p$Florence <- as.factor(full_p$Florence)
full_p$Mcode <- as.factor(full_p$Mcode)
full_p$Rcode <- as.factor(full_p$Rcode)
full_p$Fcode <- as.factor(full_p$Fcode)
head(full_p)
set.seed(2)
train.index1 <- sample(1:nrow(full_p), (1800/nrow(full_p))*nrow(full_p))
train1 <- full_p[train.index1, ]
valid1 <- full_p[-train.index1, ]
#Logistic model

## full predictors
model1 <- glm(Florence ~ ., data = train1, family = binomial)
summary(model1)
## The best judge I thought: ArtBks, ItalArt, ItalAtlas and ItalCook
model2 <- glm(Florence ~ ArtBks + ItalArt + ItalAtlas + ItalCook, data = train1, family = binomial)
summary(model2)
## RFM
model3 <- glm(Florence ~ Rcode + Fcode+ Mcode, data = train1, family = binomial)
summary(model3)
## cutoff criterion for a campaign is a 30% likelihood of apurchase
library(caret)
mydf$response1 <- as.factor(ifelse(mydf$val1 > 0.3, 1, 0))
mydf$response2 <- as.factor(ifelse(mydf$val2 > 0.3, 1, 0))
mydf$response3 <- as.factor(ifelse(mydf$val3 > 0.3, 1, 0))


z1 <- confusionMatrix(mydf$response1, mydf$Florence)
z2 <- confusionMatrix(mydf$response2, mydf$Florence)
z3 <- confusionMatrix(mydf$response3, mydf$Florence)

z1$table; z2$table ;z3$table
