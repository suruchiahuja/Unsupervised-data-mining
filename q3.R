rm(list = ls())
graphics.off()

library("arules")
library(MASS)
data <- Boston
View(data)

x11()
par(mfrow=c(5,3), mar=c(2, 2, 1, 0))
for (i in 1:ncol(Boston)){
  hist(Boston[, i], main=colnames(Boston)[i], breaks="FD")
  }

head(data)

data[["chas"]] <- NULL

#deal with ordered variables
data[["age"]] <- ordered(cut(data[["age"]], c(0, 25, 45, 65, 101)), labels = c("Young", "Middle-aged", "Senior", "Elderly"))
data[["crim"]] <- ordered(cut(data[["crim"]], c(0, 10, 30, 50, 90.00)), labels = c("very-low crime", "low crime", "medium crime", "high crime"))
data[["zn"]] <- ordered(cut(data[["zn"]], c(-5, 20, 40, 80, 101)), labels = c("very-low", "low", "medium", "high"))
data[["indus"]] <- ordered(cut(data[["indus"]], c(0, 7, 12, 20, 28)), labels = c("very-low", "low", "medium", "high"))
data[["nox"]] <- ordered(cut(data[["nox"]], c(0.1, 0.2, 0.6, 0.8, 1.0)), labels = c( "low", "medium ", "high conc"))
data[["rm"]] <- ordered(cut(data[["rm"]], c(2, 4, 6, 8, 10)), labels = c("very-low num", "low num", "medium num", "high num"))
data[["dis"]] <- ordered(cut(data[["dis"]], c(0, 4, 6, 8, 13)), labels = c("very-low dist", "low dist", "medium dist", "high dist"))
data[["rad"]] <- ordered(cut(data[["rad"]], c(0, 5, 12, 18, 25)), labels = c( "low", "medium", "high"))
data[["tax"]] <- ordered(cut(data[["tax"]], c(180,300, 500, 600, 750 )), labels = c( "low", "medium", "high"))
data[["ptratio"]] <- ordered(cut(data[["ptratio"]], c(12, 16, 18, 20, 23)), labels = c("very-low", "low", "medium", "high"))
data[["black"]] <- ordered(cut(data[["black"]], c(0, 100, 200, 300, 400)), labels = c("very-low", "low", "medium", "high"))
data[["lstat"]] <- ordered(cut(data[["lstat"]], c(0, 10, 20, 30, 40)), labels = c("very-low", "low", "medium", "high"))
data[["medv"]] <- ordered(cut(data[["medv"]], c(0, 10, 25, 40, 60)), labels = c("very-low", "low", "medium", "high"))

#convert to a binary incidence matrix
data1 <- as(data, "transactions")
summary(data1)

x11()
itemFrequencyPlot(data1, support = 0.05, cex.names = 0.8)

# Apply the apriori algorithm
rules  <- apriori(data1, parameter = list(support = 0.001, confidence = 0.6))
summary(rules)

rulescrimSmall <- subset(rules, subset = rhs %in% "crim=low crime" & lift > 1.2)
rulescrimdisSmall <- subset(rulescrimSmall, subset = lhs %in% "dis=very-low dist" & lift > 1.2)
rulesptratioSmall <- subset(rules, subset = rhs %in% "ptratio=low" & lift>1.2)

rulescrimSmall
rulescrimdisSmall
rulesptratioSmall

inspect(head(sort(rulescrimSmall, by = "confidence"), n = 10))
inspect(head(sort(rulescrimdisSmall, by = "confidence"), n = 10))
inspect(head(sort(rulesptratioSmall, by = "confidence"), n = 10))

inspect(head(sort(rulescrimSmall, by = "lift"), n = 3))
inspect(head(sort(rulesptratioSmall, by = "lift"), n = 3))

