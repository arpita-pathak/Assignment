library(arules)
library(arulesViz)
data <- read.csv(file.choose())
data <- data[6:15]
class(data)
colnames(data)
data$Sixth.Sense <- factor(data$Sixth.Sense,levels = c("1","0"),labels = c("Sixth.Sense",""))
data$Gladiator <- factor(data$Gladiator,levels = c("1","0"),labels = c("Gladiator",""))
data$LOTR1 <- factor(data$LOTR1,levels = c("1","0"),labels = c("LOTR1",""))
data$Harry.Potter1 <- factor(data$Harry.Potter1,levels = c("1","0"),labels = c("Harry.Potter1",""))
data$Patriot <- factor(data$Patriot,levels = c("1","0"),labels = c("Patriot",""))
data$LOTR2 <- factor(data$LOTR2,levels = c("1","0"),labels = c("LOTR2",""))
data$Harry.Potter2 <- factor(data$Harry.Potter2,levels = c("1","0"),labels = c("Harry.Potter2",""))
data$LOTR <- factor(data$LOTR,levels = c("1","0"),labels = c("LOTR",""))
data$Braveheart <- factor(data$Braveheart,levels = c("1","0"),labels = c("Braveheart",""))
data$Green.Mile <- factor(data$Green.Mile,levels = c("1","0"),labels = c("Green.Mile",""))

#library(car)
#library(carData)
#library(mvinfluence)

data1 <- as(data,"transactions")

itemFrequencyPlot(data1,topN=15)
rules <- apriori(data1, parameter = list(supp = 0.005, confidence = 0.50, minlen = 2, maxlen = 3))
inspect(head(sort(rules), n = 10))
plot(head(sort(rules, by = "lift"), n = 10), method = "graph", control = list(cex = 1.0)) #cex = font size
plot(rules)
plot(head(sort(rules), n = 10), method = "grouped", control = list(cex = 0.3))
