## So as it turns out, we can in fact customize the groupings for
## crossvalidation in the R pls package. We do this by supplying a
## "segments" argument to plsr.

## See: ?mvrcv, ?cvsegments, ?plsr

## Our approach here to manage the groups is to create a lookup table
## that maps sample id number to spot row in the matrix supplied to
## PLS. Like so:

## lookup.table <- list(1:10,11:20,21:30,31:40,41:50)

## Having created sample# to spot row# lookup table, we use cvsegments
## on the table to randomly assign samples consistently to folds.

## In this case, 10 would be the number of samples. The inner lapply
## maps those results to the spot rows, and the outer lapply formats
## it corrects to be used as a "segments" argument.

# lapply(lapply(cvsegments(10,5,type="random"),function(x){lookup.table[x]}),unlist)


## Motivation: uncover whether PLS can "average" over spots better
## than we can. For comparison's sake with the rest of the century
## set, use LOO.

## Is LOO optimistic? Maybe.
