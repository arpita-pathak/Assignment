library(arules)
class(Groceries)
inspect(head(Groceries, 3))
tdata <- read.transactions("transactions_data.txt", sep="\t")
tData <- as (myDataFrame, "transactions") # convert to 'transactions' class
size(head(Groceries)) # number of items in each observation
LIST(head(Groceries, 3)) # convert 'transactions' to a list, note the LIST in CAPS
frequentItems <- eclat (Groceries, parameter = list(supp = 0.07, maxlen = 15)) # calculates support for frequent items
inspect(frequentItems)
itemFrequencyPlot(Groceries, topN=10, type="absolute", main="Item Frequency") # plot frequent items
rules <- apriori (Groceries, parameter = list(supp = 0.001, conf = 0.5)) # Min Support as 0.001, confidence as 0.8.
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf)) # show the support, lift and confidence for all rules
rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift)) # show the support, lift and confidence for all rules
rules <- apriori(Groceries, parameter = list (supp = 0.001, conf = 0.5, maxlen=3))
subsetRules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector
length(subsetRules)  #> 3913
rules <- rules[-subsetRules] # remove subset rules. 
rules <- apriori (data=Groceries, parameter=list (supp=0.001,conf = 0.08), appearance = list (default="lhs",rhs="whole milk"), control = list (verbose=F)) # get rules that lead to buying 'whole milk'
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))
rules <- apriori (data=Groceries, parameter=list (supp=0.001,conf = 0.15,minlen=2), appearance = list(default="rhs",lhs="whole milk"), control = list (verbose=F)) # those who bought 'milk' also bought..
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))