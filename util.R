## Assorted useful functions that various models would want to have
## access to.

killNA <- function(comps.xm.hs,element.name){
    my.elt <- !is.na(comps.xm.hs@data[,element.name])
    comps.xm.hs[my.elt]
}

TakeIndicies <- function(matrix,is,js){
  mapply(function(i,j){matrix[,i,j]},is,js)
}

AlphabeticHyperspec <- function(x.hs){
  ## this will sort by sample name to bring the hs object in line with
  ## the order of the comps file. it is important to make sure these
  ## things are ordred the same way
  x.hs[sort(as.character(x.hs$id),index.return=TRUE)$ix,]
}
