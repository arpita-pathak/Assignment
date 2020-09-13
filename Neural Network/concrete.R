library(ggvis) #Data visulization
library(psych) #Scatterplot matrix
library(knitr) #html table
library(neuralnet) #artifical neural network 
concrete <- read.csv(file.choose() )
knitr::kable(head(concrete), caption = "Partial Table Preview")
str(concrete)
concrete %>% ggvis(x = ~strength, fill:= "#27bc9c") %>% layer_histograms() %>% layer_paths(y = ~strength, 35.82, stroke := "red")
pairs.panels(concrete[c("cement", "slag", "ash", "strength")])
normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x) ))
}

concrete_norm <- as.data.frame(lapply(concrete, normalize))
A preview of the normalized concrete dataset:
  
  kable(round(head(concrete_norm), digits = 3), caption = "Normalized Data Preview")
#training set
concrete_train <- concrete_norm[1:773, ]

#test set
concrete_test <- concrete_norm[774:1030, ]
#Build a neural network with one hidden layer 
concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age , data = concrete_train, hidden = 1)
plot(concrete_model)
#building the predictor, exclude the target variable column
model_results <- compute(concrete_model, concrete_test[1:8])

#store the net.results column 
predicted_strength <- model_results$net.result
cor(predicted_strength, concrete_test$strength)
#building the new model
concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data = concrete_train, hidden = 5 )
#nuilding the new predictor
model_results2 <- compute(concrete_model2, concrete_test[1:8])

#storing the results
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)
