#'wiki_graph
#'
#' A dataset containing the graph data extracted from a Wikipedia page.
#' @export
#' @format A data frame with the following columns:
#' \describe{
#'   \item{v1}{The edges of the graph}
#'   \item{v2}{The edges of the graph}
#'   \item{w}{The weight of the edge}
#' }
#' @source \url{https://en.wikipedia.org/wiki/Graph}
#' @references Wikipedia: Graph
"wiki_graph"
wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))