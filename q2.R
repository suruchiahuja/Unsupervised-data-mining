rm(list = ls())
graphics.off()

library("multtest")
library("fpc")
library("cluster")
library("stats")
library("BiocGenerics")

#Generating a simulation
set.seed(1234)
data <- matrix(rnorm(20*3*50, mean=0, sd=0.001), ncol=50)
true_labels <- c(rep(1, 20), rep(2, 20), rep(3, 20))

#Separate the three classes amongst two dimensions
data[1:20, 1] <- data[1:20, 1] + 1.0
data[21:40, 2] <- data[21:40, 2] + 3.5
data[41:60, 1] <- data[41:60, 1] + 5.5

#Principal component analysis
pca.fit = prcomp(data)
summary(pca.fit)
x11()
plot(pca.fit)
x11()
pca.fit$x[,1:2]
plot(pca.fit$x[,1:2], col=2:4, xlab="Z1", ylab="Z2", pch=16) 

#k-means clustering 
#k=3
k.fit <- kmeans(data, 3, nstart = 20)
k.fit
summary(k.fit)
table(true_labels,k.fit$cluster)

#k=2 and k=4
k.fit = kmeans(data, 2, nstart=20)
k.fit$cluster
table(true_labels, k.fit$cluster)

k.fit = kmeans(data, 4, nstart=20)
k.fit$cluster
table(true_labels,k.fit$cluster)

#k=3 , kmeans on the first two principal score vectors
k.fit = kmeans(pca.fit$x[,1:2], 3, nstart = 20)
k.fit$cluster

table(true_labels,k.fit$cluster)

#scaling the columns of x
data1 <- scale(data, center = TRUE, scale = TRUE)
k.fit <- kmeans(data1, 3, nstart=20)
k.fit$cluster
table(true_labels,k.fit$cluster)



