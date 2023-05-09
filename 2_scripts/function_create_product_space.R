#### Function to create product space or region space with defined threshold 
#### Function to create product space or region space with defined threshold 
#### Function to create product space or region space with defined threshold 

library(igraph)


complexity_graph <- function(proximity_mat, threshold){
    
    proximity_mat <- (-1) * proximity_mat
    g <- graph_from_adjacency_matrix(proximity_mat, weighted = TRUE, 
                                     mode = "undirected", diag = FALSE)
    E(g)$mst <- 0
    
    g_mst <- mst(g, algorithm = "prim")
    E(g_mst)$mst <- 1
    
    g_not_in_mst <- delete.edges(g, which(abs(E(g)$weight) <= threshold))
    g_not_in_mst <- graph.difference(g_not_in_mst, g_mst)
    
    g <- graph.union(g_mst, g_not_in_mst)
    E(g)$weight <- pmin(E(g)$weight_1, E(g)$weight_2, 
                        na.rm = T)
    E(g)$mst <- pmax(E(g)$mst_1, E(g)$mst_2, 
                     na.rm = T)
    
    g <- remove.edge.attribute(g, "weight_1")
    g <- remove.edge.attribute(g, "weight_2")
    g <- remove.edge.attribute(g, "mst_1")
    g <- remove.edge.attribute(g, "mst_2")
    
    E(g)$weight <- (-1) * E(g)$weight
    return(g)
    
    
}




