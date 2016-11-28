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
