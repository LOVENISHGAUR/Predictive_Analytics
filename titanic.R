load("/Users/lovenishgaur/Documents/Predictive_Analytics_IIM_Raipur/CourseMaterial/Association Mining/Titanic/titanic.raw.rdata")
setwd("/Users/lovenishgaur/Documents/Predictive_Analytics_IIM_Raipur/CourseMaterial/Association Mining/Titanic")

load("titanic.raw.rdata")
head(titanic.raw)
attach(titanic.raw)

str(titanic.raw)

install.packages("Matrix")
install.packages("arules")

library(Matrix)
library(arules)
data = titanic.raw
str(data)


#Association Rule Mining
rules = apriori(titanic.raw)
inspect(rules)
?apriori
rules = apriori(titanic.raw, parameter = list(minlen=2, supp=0.005, conf=0.8), appearance = list(rhs=c("Survived=No","Survived=Yes"), default="lhs"), control = list(verbose=F))
rules.sorted = sort(rules, by="lift")
inspect(rules.sorted)

#Pruning redundant rules
- Find redundant rules

subset.matrix = is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] = FALSE
redundant = colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

- remove redundant rules
rules.pruned = rules.sorted[!redundant]
inspect(rules.pruned)

#Visualizing Association Rules
install.packages("arulesViz")
library(arulesViz)

plot(rules)
plot(rules.pruned)
plot(rules, method="graph", control=list(type="items"))
plot(rules.pruned, method="graph", control=list(type="items"))
plot(rules, method="paracoord", control=list(reorder=TRUE))
plot(rules.pruned, method="grouped")

