library(readr)
library(ggraph)
library(igraph)
library(tidygraph)
library(visNetwork)
library(ggnetwork)
library(tidyverse)
library(network)

marvel <- read_csv("/Users/dunk/Downloads/marvel.csv")
view(marvel)


nw <- network(marvel, directed = T, matrix.type = "edgelist")

R<-network::network(marvel, directed = FALSE, matrix.type = "edgelist")

#node attributes
set.vertex.attribute(R, "between", sna::betweenness(R))
set.vertex.attribute(R, "close", sna::closeness(R))
set.vertex.attribute(R, "count", sna::degree(R))

#about that edge
set.edge.attribute(R, "how", marvel$HOW)

#target layout - arranged by degree, labels offset 5 from dots, outliers way out side
L<-ggnetwork(R, layout = "target", "between", circ.lab.offset=5, periph.outside=TRUE)

#eigenvector based layourt using the strong assumptions, you can switch to weak if you want
K<-ggnetwork(R, layout = "eigen", "symstrong")

#area is size, cool controls Simulated Annealing - bigger numbers bigger crystals
P<-ggnetwork(R, layout = "fruchtermanreingold", area = 100, cool=2)


ggplot(K, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(colour=how)) +
  geom_nodes(aes(size = between, alpha = 1/count)) +
  geom_nodetext_repel(aes(label = vertex.names))+
  theme_blank() +
  theme(legend.position = "bottom")





x <- marvel$HOW
nw %v% "HOW" = as.character(x)
y = RColorBrewer::brewer.pal(7, "Blues")[ c(3, 1, 6, 5, 2) ]
names(y) = levels(x)
# Colorful networking
GGally::ggnet2(nw, label = T, label.size = 3, arrow.size = 3, arrow.gap = 0.03,
               color = "forestgreen", palette = y, alpha = 0.5, edge.color = "black", size = "HOW",
               size.legend = "Centrality")


# Now some real networking
nw2 <- network(nw, directed = T, matrix.type = "edgelist")

GGally::ggnet2(nw, label = T, label.size = 3, arrow.size = 3, arrow.gap = 0.02, color = "HOW",
               color.legend = "Modularity", alpha = 0.5)



