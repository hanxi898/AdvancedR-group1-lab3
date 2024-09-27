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
