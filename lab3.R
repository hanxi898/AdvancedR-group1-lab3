#'euclidean: 
#
#' @param a The first number.
#' @param b The second number. 
#' @description This function computes the greatest common divisor of two numbers.
#' @return The greatest common divisor of two numbers.
#' @references https://en.wikipedia.org/wiki/Euclidean_algorithm
euclidean<- function(a,b){
  stopifnot(is.numeric(a), is.numeric(b), length(a)==1, length(b)==1)
  c<- 1
  while(c!=0){
    c<- a %% b
    a<- b
    b<- c
  }
  return(a)
}

#'dijkstra: 
#
#' @param graph A data frame representing the graph, with columns for edges and weights.
#' @param init_node The starting node for the algorithm. 
#' @description This function computes the shortest paths from a given starting node to all other nodes in a graph.
#' @return A vector of shortest distances from the starting node to all other nodes.
#' @references https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
dijkstra <- function(graph,init_node){
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

package.skeleton(name = "group1lab3")

getwd()
setwd("C:/Users/0529y/Desktop/semester1/Advanced Programming in R/Git/AdvancedR-group1-lab3/group1lab3")
devtools::document()  
devtools::load_all()
rm(list = c("dijkstra",
            "euclidean"))
rm(list = ls())
install.packages("roxygen2")

library(devtools)
devtools::document()

usethis::use_github_action("check-standard")

