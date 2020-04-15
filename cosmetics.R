setwd("/Volumes/Transcend/Teaching/IIM Raipur/2017-18-IIM Raipur-PGP-BAUDM/Lecture Notes/4. Clustering and Association Techniques/Association Rule Mining")
getwd()
install.packages("xlsx")
library(xlsx)
library(xlsReadWrite)
data = read.xlsx("Cosmetics.xls", sheetName = "Sheet1")
data = read.csv("Cosmetics.csv")
str(data)
data = subset(data[,2:15])
str(data)

i=1
for (i in 1:14)
{
data[,i] = as.factor(data[,i])	
i=i+1
}

install.packages("Matrix")
install.packages("arules")
library(Matrix)
library(arules)

#Association Rule Mining
rules = apriori(data)
inspect(rules)
rules = apriori(data, appearance = list(rhs=c("Lipstick=1"),lhs=c("Lip.liner=1")), control=list(verbose=F))

rules.sorted = sort(rules, by="lift")
inspect(rules.sorted)


#Pruning redundant rules
- Find redundant rules

subset.matrix = is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] = NA
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

