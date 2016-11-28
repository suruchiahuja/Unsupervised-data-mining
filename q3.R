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




