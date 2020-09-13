require(tree)
require(ISLR)
data(Carseats)
head(Carseats)
str(Carseats)
attach(Carseats)
High <- ifelse(Sales <= 8, "No", "Yes")
Carseats <- data.frame(Carseats, High)
head(Carseats)
tree.carseats <- tree(High ~. -Sales, Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)
tree.carseats
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
carseats.test <- Carseats[-train, ]
High.test <- High[-train]
tree.carseats.train <- tree(High ~. -Sales, Carseats, subset = train)
tree.pred <- predict(tree.carseats.train, carseats.test, type = "class")
table(tree.pred, High.test)
(86 + 57)/200
set.seed(3)
cv.carseats <- cv.tree(tree.carseats.train, FUN = prune.misclass)
names(cv.carseats)
cv.carseats
#We plot the error rate as a function of both size and k.
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")
tree.pred1 <- predict(prune.carseats, carseats.test, type = "class")
table(tree.pred1, High.test)
(94 + 60)/200

