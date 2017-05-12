# Social network analysis 

library(sna) # social network analysis 
library(igraph)

txt1 <- read.table("C:/Users/sherr/Desktop/R/undirected.txt")

head(txt1)

graph1 <- graph_from_data_frame(txt1, directed = FALSE,
                               vertices = NULL) 
plot(graph1)