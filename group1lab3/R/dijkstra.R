#'dijkstra: 
#
#' @param graph A data frame representing the graph, with columns for edges and weights.
#' @param init_node The starting node for the algorithm. 
#' @description This function computes the shortest paths from a given starting node to all other nodes in a graph.
#' @return A vector of shortest distances from the starting node to all other nodes.
#' @references https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
dijkstra <-
function(graph,init_node){
  g <- graph_from_data_frame(graph, directed = TRUE) 
  required <- c("v1", "v2", "w")
  stopifnot(is.data.frame(graph))
  stopifnot(all(required %in% names(graph)) )
  stopifnot(is.numeric(init_node) , length(init_node)==1)
  stopifnot(init_node %in% V(g))
  shortest_distance <- distances(g, v = init_node, to = V(g), weights = E(g)$w, algorithm = "dijkstra")
  sd <- as.vector(shortest_distance)
  return(sd)
}
