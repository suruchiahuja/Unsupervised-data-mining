rm(list = ls())
graphics.off()

data <- read.delim("C:/Users/Suruchi Ahuja/Desktop/R prog/New folder/546 hw4/heart-2.txt")
View(data)

data.fit <- hc(data)
x11()
graphviz.plot(data.fit)

data.fit <- set.arc(data.fit,"family","systol")
data.fit <- set.arc(data.fit,"mental","smoke")
data.fit <- drop.arc(data.fit,"mental","phys")

data.model <- bn.fit(data.fit, data)
tables <- as.grain(data.model)
data.model
tables
find <- setFinding(tables, nodes = c("systol"),states=c("y"))
q3 <- querygrain(find,nodes = c("mental","phys"),type="marginal")
