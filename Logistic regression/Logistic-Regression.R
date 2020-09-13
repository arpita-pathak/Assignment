#Classify whether application accepted or not using Logistic regression
#card
#Factor. Was the application for a credit card accepted?
#reports
#Number of major derogatory reports.
#age
#Age in years plus twelfths of a year.
#income
#Yearly income (in USD 10,000).
#share
#Ratio of monthly credit card expenditure to yearly income.
#expenditure
#Average monthly credit card expenditure.
#owner
#Factor. Does the individual own their home?
#selfemp
#Factor. Is the individual self-employed?
#dependents
#Number of dependents.
#months
#Months living at current address.
#majorcards
#Number of major credit cards held.
#active
#Number of active credit accounts.

credit_data <- read.csv(file.choose())
View(credit_data)
creditData <- credit_data[-1]

summary(creditData)
sum(is.na(creditData)) ## no missing value available
str(creditData)
attach(creditData)
## Build first logistic model

creditModel <- glm(card ~ .,creditData,family = "binomial")
summary(creditModel) 
## here null deviance 1404.6 is lesser than residual deviance 13336.2 so we can not aceept this model

## checking corelation between all variables
library(corpcor)
pairs(creditData,col="blue")
cor2pcor(cor(creditData[-c(1,7,8)]))
pairs(creditData[-c(1,7,8)]) ## avoiding categorical data

## form this it is observed that share and expenditure has strong relationship
##0.88837610

## now will build model without expenditure 

#creditModel_Exp <- glm(creditData$card ~ .-creditData$expenditure,data=creditData,family = "binomial")
#summary(creditModel_Exp)         

creditData_Exp <- creditData[-6]

## build 2nd model without expenditure

creditModel_Exp1 <- glm(creditData_Exp$card ~ . ,data = creditData_Exp,family = "binomial")
summary(creditModel_Exp1)

## from 2nd model it is observed that age,owner,selfempy,months,majorcards are insignificant

## will build 3rd model with only significant variables
attach(creditData_Exp)
creditmodel_final <- glm(card ~ reports+income+share+dependents+active,data = creditData_Exp,family = "binomial" )
summary(creditmodel_final)

## in this final model null deviance 1404.57 is more than residual variance 136.96 and all variables are significant
## so we can use this model 

prob <- creditmodel_final$fitted.values
prob

prob1 <- predict(creditmodel_final,type = "response",credit_data)
prob1

## confusion Matrix

confusion <- table(prob>0.50,credit_data$card)
confusion ## false negative i.e. alpha value is 23

## Model Accuracy
accuracy_credit <- sum(diag(confusion)/sum(confusion))
accuracy_credit  # accuracy is 0.9810462 i.e.98.10 %

error_model = 1- accuracy_credit
error_model     # error for this model is 0.01895375 

prop.table(table(credit_data$card))
## original proportion
###     no       yes 
## 0.2244124 0.7755876 

prop.table(table(prob>0.50))

## predicted proportion
# FALSE      TRUE 
# 0.2403336 0.7596664 

## ROC Curve
install.packages("ROCR")
library(gplots)
library(ROCR)
roc_predict <- prediction(prob,creditData$card)
roc_perform <- performance(roc_predict,'tpr','fpr')
str(roc_perform)

## plot roc curve

plot(roc_perform,colorize = T,text.adj=c(-0.2,1.7),print.cutoffs.at=seq(0.1,by=0.1))

## more area under the curve better is the model obtained

## build data frame for tpr and fpr values

rocr_cutoff <- data.frame(cut_off =roc_perform@alpha.values[[1]],fpr=roc_perform@x.values,tpr=roc_perform@y.values)
View(rocr_cutoff)

colnames(rocr_cutoff) <- c("Cut off","FPR","TPR")

## Rounding values to two decimals 

rocr_cutoff<-round(rocr_cutoff,2) ## cutoff value we can consider is 0.44

confusion2 <- table(prob >0.44,credit_data$card)
confusion2  ## alpha is 21

accuracy <- sum(diag(confusion2)/sum(confusion2))
accuracy ## accuracy is 0.9818044


