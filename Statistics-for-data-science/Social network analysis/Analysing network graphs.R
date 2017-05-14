# Analysing network graphs 
library(igraph)


# preferential attachment network 
graph1 <- sample_pa(12, power = 1, directed = FALSE)
		plot(graph1)

# Degrees
# Degree shows the number of linkes for each node 
degree(graph1) # Degree is used to find out the most popular individuals in the network 
# [1] 4 2 3 4 1 1 2 1 1 1 1 1
# 1 and 4 are the most popular nodes, each have 4 links 


# Betweenness 
# Centrality measure of a vertex within a graph  
graph2 <- sample_pa(20, power=1, directed = FALSE)
		plot(graph2)
		betweenness(graph2) # The higher the number the higher the centrality/connections
# [1]   0  18  34  89 111  18   0 105  65  35   0  18   0   0  18   0   0   0   0
# [20]  0
# nodes 5 and 8 have the highest beetweenness (111 and 105)


# Network density 
# Number of connections/number of possible connections 
graph3 <- sample_pa(12, power = 1, directed = FALSE)
		Dens <- edge_density(graph3, loops = FALSE)
		Dens # 1.667
# Try 20 nodes 
graph4 <- sample_pa(20, power = 1, directed = FALSE)
		Dens1 <- edge_density(graph4, loops = FALSE)
		Dens1 # 0.1
# Try 40 nodes 
graph5 <- sample_pa(40, power = 1, directed = FALSE)
		Dens2 <- edge_density(graph5, loops = FALSE)
		Dens2 # 0.05





