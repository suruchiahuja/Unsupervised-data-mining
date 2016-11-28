#####################################
##### QUESTION 1 
####################################
rm(list = ls())
graphics.off()

#install.packages("devtools")
library("stats")
library("mclust")
#library("ggbiplot")

#loading the data
load("C:/Users/Suruchi Ahuja/Downloads/SwissBankNotes.RData")
data <- SwissBankNotes
head(data)

genuine <- data[1:100,]
counterfiet <- data[101:200,]

#Principal component analysis on the bank notes
pr.fit <- prcomp(scale(data), center = FALSE, scale = FALSE)
x11()
plot(pr.fit)
summary(pr.fit)
x11()
biplot(pr.fit,cex=c(2,1), col=c("red","blue"))

#Principal component analysis on 100 genuine bank notes
pr.fit1 <- prcomp(scale(genuine), center = FALSE, scale = FALSE)
x11()
plot(pr.fit1)
summary(pr.fit1)
x11()
biplot(pr.fit1, col=c("red","yellow"))

#Principal component analysis on 100 counterfiet bank notes
pr.fit2 <- prcomp(scale(counterfiet), center = FALSE, scale = FALSE)
x11()
plot(pr.fit2)
summary(pr.fit2)
x11()
biplot(pr.fit2, cex=c(2,1), col=c("red","green"))



################################################################
########### QUESTION 2
###############################################################
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




#############################################################
############  Question 3
###########################################################
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



################################################################################################
###############################################################################################
####################################################################################################