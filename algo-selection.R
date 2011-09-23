## The purpose of this file is to provide a workspace for algorithmic
## selection of training sets for PLS.

## mean spectrum is: apply(x.hs,2,mean)

## give the relevent hyperspec object as a vector:
## as.vector(sed.mean.hs[1][[]]) 

## give a hyperspec matrix where spc is now the cosine distance to vector sead.mean.hs
## for each row
## apply(cset.hs,1,function(x){cosine(as.vector(sed.mean.hs[1][[]]),as.vector(x))})

## take those means into a data frame all their own
## easy.hat <- data.frame(means$id,means[[]])

## now sort that data frame by the column that came into it from "means"
##  easy.hat[with(easy.hat,order(-X1)),]

## While we're at it we should really use PredReady to make a "model
## training and assessment function" to just drop out the sorts of
## measurement we're going to want to be doing on our samples.

## cos.sorted.cset.hs <- cset.hs[cos.sorted.hat$means.id,]

## An easy modelbuild function will do...

SpectraModel <- function(x.hs,x.comps,n.comp){
  pred.df <- PredReady(x.hs,x.comps)
  plsr(comps ~ spectra ,ncomp=n.comp,data=pred.df)
}


## same analysis as above (which really didn't work out too well, try
## another distance metric) with "clustering" on the response
## variables (compositions):

## mean.sed.composition <- apply(sed.comps,2,mean)
## > composition.cos.distances <- apply(cset.comps,1,function(x){cosine(as.vector(mean.sed.composition),as.vector(x))})

## sorted.comp.dist <- composition.cos.distances[order(composition.cos.distances)]
## > orderit<- data.frame(ids=attr(sorted.comp.dist,"names"))
## > comp.or.cset <- cset.hs[orderit$ids,]
