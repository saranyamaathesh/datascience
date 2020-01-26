###association rules groceries 


library(arules)
groceries<-read.transactions(file.choose(),format="basket")
View(groceries)
inspect(groceries[1:10])
class(groceries)
itemFrequencyPlot(groceries,topN=20)
groceries_rules<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.7,minlen=3))
library(arulesViz)
plot(groceries_rules,method = "scatterplot")
plot(groceries_rules,method = "grouped")
plot(groceries_rules,method = "graph")
plot(groceries_rules,method = "mosaic")
###hence for confidence=0.05,minlen=3 it gives 108 rules
###hence for confidence=0.5,minlen=3 it gives 55 rules
###hence for confidence=0.7,minlen=3 it gives 45 rules
   


##association rules books


install.packages("tm")
book <- read.csv(file.choose())
install.packages("arules")
install.packages("arulesviz")
library(arules)
library(arulesViz)
qq=as.matrix(book)
qq=as(qq,"transactions")
rules <- apriori(qq,parameter = list(support=0.10,confidence=0.5,minlen=3))
rules=sort(rules,by="lift")
inspect(rules)
plot(rules,method = "graph")
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "mosaic")
###hence for confidence=0.5,minlen=2 it gives 44 rules
###hence for confidence=0.05,minlen=2 it gives 70 rules
###hence for confidence=0.5,minlen=3 it gives 28 rules
