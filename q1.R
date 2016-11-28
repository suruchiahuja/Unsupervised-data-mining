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




