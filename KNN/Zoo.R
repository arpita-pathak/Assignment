set.seed(1)
library(class)
d = read.table("zoo.DATA", sep=",", header = FALSE)
d = data.frame(d)
  names(d) <- c("animal", "hair", "feathers", "eggs", "milk", "airborne",
                "aquatic", "predator", "toothed", "backbone", "breathes", "venomous",
                "fins", "legs", "tail", "domestic", "size", "type")

types <- table(d$type)
d_target <- d[, 18]
d_key <- d[, 1]
d$animal <- NULL
names(types) <- c("mammal", "bird", "reptile", "fish", "amphibian", "insect", "crustacean")
types
summary(d)
str(d)
k = sqrt(17) + 1
m1 <- knn.cv(d, d_target, k, prob = TRUE)
prediction <- m1
cmat <- table(d_target,prediction)
acc <- (sum(diag(cmat)) / length(d_target)) * 100
print(acc)
Confusion Matrix:
data.frame(types)
cmat
Accuracy (%)
acc

