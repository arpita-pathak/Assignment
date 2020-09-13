#Calories_consumed-> predict weight gained using calories consumed
wt.cal <- read.csv(file.choose())
attach(wt.cal)
summary(wt.cal)
str(wt.cal)
plot(Weight.gained..grams.,Calories.Consumed)
cor(Weight.gained..grams.,Calories.Consumed)
reg<-lm(Calories.Consumed~Weight.gained..grams.)
new.weight<-data.frame(Weight.gained..grams.)
predict(reg,newdata=new.weight )


#Delivery_time -> Predict delivery time using sorting time 
dt.st <- read.csv(file.choose())
attach(dt.st)
str(dt.st)
plot(Delivery.Time,Sorting.Time)
cor(Delivery.Time,Sorting.Time)
reg<-lm(Sorting.Time~Delivery.Time)
new.time<-data.frame(Delivery.Time)
predict(reg,newdata=new.time )


#alary_hike -> Build a prediction model for Salary_hike
sal <- read.csv(file.choose())
attach(sal)
str(sal)
plot(Salary, YearsExperience)
sum(is.na(sal))
boxplot(sal$Sal, horizontal = T)
boxplot(sal$YearsExperience, horizontal = T)
library(lattice)
dotplot(sal$Sal, main="Dotplot of Sal")
dotplot(sal$YearsExperience, main="Dotplot of Years of Experience")
qqnorm(sal$Sal, main = "Sal")
qqline(sal$Sal)
reg<-lm(Salary~YearsExperience)
reg$residuals
reg$fitted.values
reg$coefficients
summary(reg)



#Emp_data -> Build a prediction model for Churn_out_rate 
emp<-read.csv(file.choose())
attach(emp)
View(emp)
plot(Churn_out_rate,Salary_hike)
cor(Churn_out_rate, Salary_hike)
sum(is.na(emp))
boxplot(emp$Salary_hike, horizontal = T)
boxplot(emp$Churn_out_rate, horizontal = T)
library(lattice)
dotplot(emp$Salary_hike, main="Dotplot of Salary Hike")
dotplot(emp$Churn_out_rate, main="Dotplot of Churn Out Rate")
qqnorm(emp$Salary_hike, main = "salary_hike")
qqline(emp$Salary_hike)
qqnorm(employee$Churn_out_rate, main = "Churn out rate")
qqline(employee$Churn_out_rate)
reg<-lm(Salary_hike~Churn_out_rate)
reg$residuals
reg$fitted.values
reg$coefficients
summary(reg)
library(ggplot)
ggplot(data=emp, aes(x=Churn_out_rate, y=Salary_hike)) + geom_point(color='blue')+geom_line(color='red', data=employee, aes(x=Churn_out_rate, y=reg2$fitted.values))



