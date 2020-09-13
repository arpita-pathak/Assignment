computer_data <- read.csv(file.choose())
View(computer_data)
x=computer_data[,-c(1)]
View(x)
attach(x)
colnames(x)
str(x)
#creating dummy variables
x$cd=as.integer(factor(x$cd,levels = c('yes','no'),labels = c(1,0)))
x$multi=as.integer(factor(x$multi,levels = c('yes','no'),labels = c(1,0)))
x$premium=as.integer(factor(x$premium,levels = c('yes','no'),labels = c(1,0)))
View(x)
summary(x)#1st moment business decision
library(psych)
describe(x)#2nd ,3rd &4th moment business decision
library(e1071)
colnames(x)
library(GGally)
library(ggplot2)
ggplot(data = x)+geom_histogram(aes(x=price,),bins=40)
ggpairs(data = x)
pairs(x)

#multiple linear regression
model <- lm(price~.,data = x)
summary(model)                                      #R^2 0.7756
rmse <- mean(model$residuals^2)^.5                  #275.129
rmse
pred <- predict(model,newdata = x)
cor(pred,x$price)                                   #0.8807
library(car)
avPlots(model)
vif(model)                           #no colinearity problem
influenceIndexPlot(model,grid = T,id=list(n=10,cex=1.5))
influence.measures(model)
qqPlot(model)                       

#removing the influences over the given data
model2 <- lm(price~.,data = x[-c(1441,1701),])
summary(model2)                               #R^2 0.77
rmse2 <- mean(model2$residuals^2)^.5          #272
rmse2
pred2 <- predict(model2,x)
cor(pred2,x$price)                            #0.88
avPlots(model2)

#transformation technique
#log model
x=log(x[,-1])
x2 <- data.frame(x[,1],x)
colnames(x2)
View(x2)
model3 <- lm(x...1.~speed+hd+ram+screen+cd+multi+premium+ads+trend,data = x2)
summary(model3)                               #r^2 0.7426
rmse3 <- mean(model3$residuals^2)^.5
rmse3                                        #294.65
pred3 <- predict(model3,x2)
cor(pred3,x2$x...1.)                         #86.17%
influenceIndexPlot(model3,grid = T,id=list(n=10,cex=1.5))
influence2 <- as.integer(rownames(influencePlot(model3,id=list(n=5,cex=1.5))))
influence2
qqPlot(model3)
vif(model3)
#log with removing of influence
model4 <- lm(b...1.~speed+hd+ram+screen+cd+multi+premium+ads+trend,data = b2[-c(1441,1701)])
summary(model4)                             #r^2 0.7426
rmse4 <- mean(model4$residuals^2)^.5
rmse4 #294.653
pred4 <- predict(model4,b2)
cor(pred4,b2$b...1.)                       #86.17


#model2 has best (R^2,accuracy) value and lesser RMSE 
plot(model2)

