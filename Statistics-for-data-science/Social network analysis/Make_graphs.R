library(igraph)
library(sna)

# make social network graphs 

# make full graph 
Ggraph <- make_full_graph(8, directed = FALSE, loops = FALSE)
		plot(Ggraph) # 8 nodes/vertices and the graph has non-directional links 

# Ring
ring1 <- make_ring(12, directed = FALSE, 
                   mutual = FALSE, 
                   circular = TRUE)
		plot(ring1)
		# 12 nodes, undirected links, circular 

# Star 
star1 <- make_star(5, center = 1)
		plot(star1)

# gnp model 
gnp1 <- sample_gnp(20, 0.5, directed = FALSE, loops = FALSE)
		# gnp model looks at the probability for drawing an edge 
		# between two arbitrary vertices  
		plot(gnp1)

# gnm model 
gnm1 <- sample_gnm(20, 50, directed = FALSE, loops = FALSE)
		# gnm model looks at the number of edges in the graph
		plot(gnm1)

# preferential attachment
gpa1 <- sample_pa(20, power = 1)
		# power gives the edges weight 
