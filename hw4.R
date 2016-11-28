#######################################
#Question 1
########################################

rm(list = ls())
graphics.off()

#install.packages("gRbase")
#install.packages("ggm")
#install.packages("bnlearn")
library(gRbase)
library(graph)
library(gRain)
library(Rgraphviz)
library(ggm)
library(bnlearn)

data(cad1)
#View(cad1)
data <- cad1

g <- list(~Sex,~Smoker|Sex,~Inherit|Smoker,~CAD|Inherit:Hyperchol,~Hyperchol|Smoker:SuffHeartF,~SuffHeartF)
cad.dag <- dagList(g)
x11()
plot(cad.dag)

#Creating a graph in terms of nodes and edges
graph <- as(cad.dag,"graphNEL")
graph1 <- as.bn(graph)
cpt_graph <- grain(graph,data)

#Query a network to check for female with high cholestrol

query1 <- setEvidence(cpt_graph, nodes = c("Sex","Hyperchol"), states = c("Female","Yes"))
query2 <- querygrain(query1, nodes = c("SuffHeartF","CAD"), type = "marginal")

#Simulation
simulate_data <- simulate(query1, n = 500)
write.table(simulate_data, "simulateddata.txt", sep="\t")

query3 <- querygrain(cpt_graph, nodes = c("SuffHeartF","CAD"), type= "marginal")
dsep(graph1 , "Sex", "SuffHeartF","CAD")

#on simulated data
cpt_graph2 <- grain(graph, simulate_data, smooth=1)
query3 <- querygrain(cpt_graph2,nodes = c("Smoker","CAD"),type="joint")

##########################################################
#Question 2
##########################################################
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

############################################################
# Question 3
#############################################################
rm(list = ls())
graphics.off()

library(igraph)

# Construct an Igraph
nodes <- data.frame(names = c("A", "B", "C", "D","E","F"))
relations <- data.frame(
  from = c("C", "B", "B","D", "D", "E", "F"), 
  to = c("A", "C", "E","B", "E", "D", "C")
)
g <- graph.data.frame(relations, directed = TRUE, vertices = nodes)

x11()
plot(g)

pg1 <- page.rank(g, damping = 0.05)
pg2 <- page.rank(g, damping = 0.25)
pg3 <- page.rank(g, damping = 0.50)
pg4 <- page.rank(g, damping = 0.75)
pg5 <- page.rank(g, damping = 0.95)

pg1$vector
pg2$vector
pg3$vector
pg4$vector
pg5$vector


#part b

nodes1 <- data.frame(name = c("A", "B", "C", "D","E","F","G","H"))
relation <- data.frame(
  from = c("D", "E", "F","G", "H", "B", "C"), 
  to = c("B", "B", "C","C", "C", "A", "A")
)
g1 <- graph.data.frame(relation, directed = TRUE, vertices = nodes1)

x11()
plot(g1)

pg <- page.rank(g1, damping = 0.15)
pg$vector

##################################################################
# Question 4
################################################################
rm(list = ls())
graphics.off()

#install.packages("arulesViz")

library(arules)
library(arulesViz)

load("C:/Users/Suruchi Ahuja/Desktop/R prog/New folder/546 hw4/titanic.raw-2.rdata")
data <- titanic.raw
newdata <- as(data, "transactions")
#View(data)

rules  <- apriori(newdata, parameter = list(support = 0.01, confidence = 0.09))
inspect(rules)
plot(rules)
#plot(rules.sorted, method="graph", control=list(type="items"))

rules <- apriori(data,parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"),control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

#pruning the redundant laws
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)
###########################

model.nb <- naive.bayes(data,"Survived")
x11()
plot(model.nb)

fit.nb <- bn.fit(model.nb, data)
fit.nb.grain <- as.grain(fit.nb)
query <- setEvidence(fit.nb.grain, nodes=c("Survived"), states=c("No"))
query1 <- querygrain(query, nodes = c("Class","Sex"), type="joint")


#####################################################
#Question 5
########################################################3
rm(list = ls())
graphics.off()

#install.packages("glasso")
#install.packages("qgraph")

library(glasso)
library(qgraph)

#load the data
data(state)
data <- state.x77

#Hierarchical chlustering
distance <- dist(data)
fit.hc <- hclust(distance)
x11()
plot(fit.hc)
summary(fit.hc)

#PCA on the data
fit.pca <- prcomp(scale(data),center = FALSE, scale. = FALSE)
summary(fit.pca)

x11()
plot(fit.pca)

x11()
boxplot(scale(data))

pca1 <- data%*%fit.pca$rotation[,1]
pca2 <- data%*%fit.pca$rotation[,2]

x11()
plot(fit.pca$x[,1:2], col=2:4, xlab="First Principal component", ylab="second Principal Component", pch=16)

X11()
biplot(fit.pca, xlim=c(-0.30, 0.30), ylim=c(-0.05, 0.2))

#Gaussian graphical model using graphical lasso
fit.gaussian <- glasso(cov(data),rho= 0)
fit.gaussian1 <- glasso(cov(data),rho= 2)
fit.gaussian2 <- glasso(cov(data),rho= 5)
fit.gaussian3 <- glasso(cov(data),rho= 8)
fit.gaussian4 <- glasso(cov(data),rho= 12)
fit.gaussian5 <- glasso(cov(data),rho= 15)

