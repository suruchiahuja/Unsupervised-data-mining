rm(list = ls())
graphics.off()

library("multtest")
library("fpc")
library("cluster")
library("stats")

data <- read.csv("Ch10Ex11.csv", header=F)
View(data)

#Apply hierarchical clustering to the samples using correlation-based distance
data1 <- (1-cor(data))
View(data1)

d <- as.dist(data1)
hc.average <- hclust(d, method = "ave")
hc.complete <- hclust(d, method = "complete")
hc.single <- hclust(d, method = "single")

x11()
plot(hc.average)
x11()
plot(hc.complete)
x11()
plot(hc.single)

#To know which genes differ the most across the two groups we apply principal component analysis
pr.fit <- prcomp(t(data))
summary(pr.fit)
x11()
plot(pr.fit)

total.load <- apply(pr.fit$rotation, 1, sum)
index <- order(abs(total.load), decreasing = TRUE)
index[1:10]



