#'dijkstra: 
#
#' @param graph A data frame representing the graph, with columns for edges and weights.
#' @param init_node The starting node for the algorithm. 
#' @export
#' @description This function computes the shortest paths from a given starting node to all other nodes in a graph.
#' @return A vector of shortest distances from the starting node to all other nodes.
#' @references https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm

dijkstra <- function(graph, init_node) {
  
  required <- c("v1", "v2", "w")
  stopifnot(is.data.frame(graph))
  stopifnot(all(required %in% names(graph)))
  stopifnot(is.numeric(init_node), length(init_node) == 1)
  
  if (!init_node %in% unique(c(graph$v1, graph$v2))) {
    stop("The initial node must exist in the graph.")
  }
  
  nodes <- unique(c(graph$v1, graph$v2))
  distances <- rep(Inf, length(nodes))
  names(distances) <- nodes
  distances[as.character(init_node)] <- 0
  
  priority_queue <- list()
  
  priority_queue[[as.character(init_node)]] <- 0
  
  while (length(priority_queue) > 0) {
    current_node <- names(which.min(unlist(priority_queue)))
    current_distance <- priority_queue[[current_node]]
    priority_queue[[current_node]] <- NULL
    
    neighbors <- graph[graph$v1 == as.numeric(current_node), ]
    
    for (i in seq_len(nrow(neighbors))) {
      neighbor <- neighbors$v2[i]
      edge_weight <- neighbors$w[i]
      
      new_distance <- current_distance + edge_weight
      
      if (new_distance < distances[as.character(neighbor)]) {
        distances[as.character(neighbor)] <- new_distance
        priority_queue[[as.character(neighbor)]] <- new_distance
      }
    }
  }
  
  return(as.numeric(distances[as.character(nodes)]))
}
