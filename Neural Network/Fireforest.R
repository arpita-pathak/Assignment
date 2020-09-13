hist(mydata$area)
rug(mydata$area)
mydata <- mutate(mydata, y = log(area + 1))  # default is to the base e, y is lower case
hist(mydata$y)
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))  # subtract the min value in x and divide by the range of values in x.
}

mydata$temp <- normalise(mydata$temp)
mydata$rain <- normalise(mydata$rain)
mydata$RH <- normalise(mydata$RH)
mydata$wind <- normalise(mydata$wind)
Classification
SVM is better suited to a classification problem. Let's pretend we're interested in the weather conditions that give rise to small fires (arbitary set to < 5 hectares), compared to larger fires. Can we classify the type of fire we might expect to see if we send a fireman out with remote meterological data? This may help them bring the right gear.

# note, our earlier transformation was redundant, $area gives the same results
sum(mydata$area < 5)  # ln(0 + 1) = 0

sum(mydata$area >= 5)

These fires are split unevenly.

mydata$size <- NULL
mydata$size <- factor(ifelse(mydata$area < 5, 1, 0),
                      labels = c("small", "large"))


train <- sample(x = nrow(mydata), size = 400, replace = FALSE)  # sample takes place from 1:x, convenience
#test, not train, use - selection

m.poly <- ksvm(size ~ temp + RH + wind + rain,
               data = mydata[train, ],
               kernel = "polydot", C = 1)
##  Setting default kernel parameters
m.poly

m.rad <- ksvm(size ~ temp + RH + wind + rain,
              data = mydata[train, ],
              kernel = "rbfdot", C = 1)
m.rad

m.tan <- ksvm(size ~ temp + RH + wind + rain,
              data = mydata[train, ],
              kernel = "tanhdot", C = 1)
##  Setting default kernel parameters
m.tan
U
pred <- predict(m.rad, newdata = mydata[-train, ], type = "response")

table(pred, mydata[-train, "size"][[1]])  #  [[]] gives the contents of a list

confusionMatrix(table(pred, mydata[-train, "size"][[1]]), positive = "small")  # from the caret package, also need e1071 package

