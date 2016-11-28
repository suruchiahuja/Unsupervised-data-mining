rm(list = ls())
graphics.off()

#install.packages("igraph")
#install.packages("ape")
#install.packages("plyr")
#install.packages("dplyr")
library(igraph)
library(ape)
library(plyr)
library(dplyr)

nexus.get("karate")
miserable<-nexus.get("miserables")

nexus.get("dolphins")
dolphins<-nexus.get("dolphins")

#################################################################
## Finding the optimal communities in the graph
######################################################

optmum <- cluster_optimal(miserable)
print(optmum)
modularity(optmum)
length(optmum)
is_hierarchical(optmum)
V(miserable)$comm <- membership(optmum)
is_hierarchical(miserable)

X11()
plot(optmum,miserable)

set.seed(1)
hrg.fit <- fit_hrg(miserable,steps = 0)
hrg.fit
print(hrg.fit, level = 5)

#Hrg model fitting and plotting the dendograms

fit.dolphin<-fit_hrg(dolphins,steps = 0)
fit.dolphin
print(fit.dolphin, level = 5)

x11()
plot_dendrogram(hrg.fit)

x11()
plot_dendrogram(fit.dolphin)

x11()
tkplot(dolphins)

#################################################################
# PREDICTING THE MISSING EDGES
##################################################################

# Create a vector of random numbers to check the edges that are to be deleted

set.seed(1)
delete1 <- sample(159,size =length(E(dolphins))*0.05, replace = FALSE, prob = NULL )

set.seed(1)
delete1 <- sample(159,size =length(E(dolphins))*0.15, replace = FALSE, prob = NULL )

set.seed(1)
delete3 <- sample(159,size =length(E(dolphins))*0.40, replace = FALSE, prob = NULL )

# Delete the edges from the graph to get noisy datasets

dol_vertices <- fit.dolphin$names
edges1<-E(dolphins)[delete1]

graph1 <- delete_edges(dolphins, delete1)
graph1

graph2 <- delete_edges(dolphins, delete2)
graph2

graph3 <- delete_edges(dolphins, delete3)
graph3

## Fit a hrg model to the new graph with the deleted edges

hrg.dolphin <-fit_hrg(graph1,steps = 0)
hrg.dolphin2 <- fit_hrg(graph2,steps = 0)
hrg.doplhin3 <- fit_hrg(graph3,steps = 0)

numb<-c(1:62)

predict1 <- predict_edges(graph1)

predicted_edges <- dol_predict$edges
predicted_edges <- mapvalues(predicted_edges, c(numb), c(dol_vertices))

predicted_edges <- as.data.frame(predicted_edges)
colnames(predicted_edges) <- c("from","to")

edges1 <- E(dolphins)[delete1]

edge1<-NULL
edge1$Fromedge <- c("Jet",  "TR99","SN100", "Trigger","SN89","Feather","Beak") 
edge1$Toedge <- c("Quasi","Trigger","SN4","Vau","Web","Gallatin","SN96")
edge1 <- data.frame(myedges)

subset1 <- subset(predicted_edges,predicted_edges$V1 == edge1$Fromedge && predicted_edges$V2 == edge1$toedge)
subset1 <- subset(predicted_edges,predicted_edges$V1 == edge1$Fromedge )
dih_col <- which((edge1$Fromedge %in% predicted_edges$V1) && (predicted_edges$V2 %in% edge1$Toedge) )


## Predict the missing links for 15% and 40% of data deletion
dolphin.predict1 <- predict_edges(graph2)

dolphin.predict2<-predict_edges(graph3)

x11()
plot_dendrogram(dol_predict12$hrg)

########################################################################


