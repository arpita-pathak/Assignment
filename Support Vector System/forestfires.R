forestfires <- read.csv("forestfires.csv",header = TRUE, fill = TRUE)

forestfires<-forestfires[which(forestfires$area != 0),]


null <- lm(log(area + 1) ~ 1, forestfires[,c(-3, -4)])
full <- lm(log(area + 1)~., forestfires[,c(-3, -4)])
summary(full)
par(mfrow=c(2,2))
plot(full, which=c(1,2,4,5))

forestfires=forestfires[-500,]



n = nrow(forestfires)
test <- sample(1:n, round(n)/10)
forestfires.train <- forestfires[-test, ]
forestfires.test <- forestfires[test, ]
par(mfrow = c(1,2))
hist(forestfires.train$area, freq=FALSE, col="blue", main="Histogram",xlab="")
lines(density(forestfires.train$area), col="black")
title(outer=TRUE, main="\n Distribution of the burned area")
plot(ecdf(forestfires.train$area), main="Cumulative distribution",xlab="")
forestfires.train$area <- log((forestfires.train$area) + 1)
par(mfrow = c(1,2))
hist(forestfires.train$area, freq=FALSE, col="blue", main="Histogram",xlab="")
lines(density(forestfires.train$area), col="black")
plot(ecdf(forestfires.train$area), main="Cumulative distribution function",xlab="")
title(outer=TRUE, main="\n Distribution of the burned area")
are.factor <- sapply(forestfires.train, is.factor)

are.factor
library(ggplot2)
qplot(month, area, data = forestfires.train, geom = "boxplot")
library(ggplot2)
qplot(day, area, data = forestfires.train, geom = "boxplot")
factor_day <- factor(forestfires.train$day, levels=c("mon","tue","wed","thu","fri","sat","sun"))
barplot(table(factor_day),las=3)
factor_month <- factor(forestfires.train$month, levels=c("jan","feb","mar",
                                                         "apr","may","jun","jul","aug","sep","oct","nov","dec"))
barplot(table(factor_month),las=3)
library(GGally)
#ggpairs(forestfires.train, columns = which(!are.factor))
heatmap(abs(cor(forestfires.train[, !are.factor])))
n <- nrow(forestfires.train)
scope <- list(lower = terms(area ~ 1, data=forestfires.train[,c(-3, -4)]),
              upper = terms(area ~ ., data=forestfires.train[,c(-3, -4)]))
step.AIC <- step(null, scope, direction='both', trace=FALSE)
step.BIC <- step(null, scope, direction='both', k=log(n), trace=FALSE)
step.AIC
step.BIC

par(mfrow=c(2,2))
plot(step.AIC, which=c(1,2,4,5))
summary(step.AIC)

# SVM
library(e1071)
svm.model <- svm( area ~ X+Y+month+day+FFMC+DMC+DC+ISI+temp+RH+wind+rain,scale = T,
                  data=forestfires.train,kernel="radial",cost=100,gamma=0.1);
testx <- as.matrix(forestfires.test[ , -13])
svm.pred <- predict(svm.model, testx,decision.values =TRUE);

# accuray <- svm.pred - forestfires.test[, 13]/forestfires.test[, 13]

summary(svm.model)

# SVM2

library(e1071)
svm.model <-svm(area~X+Y+month+day+FFMC+DMC+DC+ISI+temp+RH+wind,scale = T,
                data=forestfires.train,kernel="radial",cost=100,gamma=0.1);
predictedarea <- predict(svm.model, forestfires.test[, -13], decision.values =TRUE);
#realarea <- as.matrix(forestfires.test[, 13])
#accuray <- (predictedarea - forestfires.test[, 13])/forestfires.test[, 13]
#fit <- table(svm.pred, testy)
#summary(svm.model)
#summary(fit)

error <- svm.model$residuals
rmse <- function(error)
  sqrt(mean(error^2))
rmse(error) 
mean(forestfires.train$area)



cor(forestfires.train$area, predict(svm.model))
accuracy <- (predict(svm.model) - forestfires.train$area)/forestfires.train$area

