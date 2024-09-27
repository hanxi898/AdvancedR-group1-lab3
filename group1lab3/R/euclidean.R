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
