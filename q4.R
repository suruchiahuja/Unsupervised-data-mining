rm(list = ls())
graphics.off()

library(arules)

load("C:/Users/Suruchi Ahuja/Desktop/R prog/New folder/546 hw4/titanic.raw-2.rdata")
data <- titanic.raw
View(data)

rules <- apriori(data)
inspect(rules)

rules <- apriori(data,parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"),control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)