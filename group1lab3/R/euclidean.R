#'euclidean: 
#
#' @param a The first number.
#' @param b The second number. 
#' @description This function computes the greatest common divisor of two numbers.
#' @return The greatest common divisor of two numbers.
#' @references https://en.wikipedia.org/wiki/Euclidean_algorithm
euclidean <-
function(a,b){
  stopifnot(is.numeric(a), is.numeric(b), length(a)==1, length(b)==1)
  c<- 1
  while(c!=0){
    c<- a %% b
    a<- b
    b<- c
  }
  return(a)
}
