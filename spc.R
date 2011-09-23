library(superpc)
## Here, we implement a supervised principal components wrapper.

## prepare a composition table and a hyperspec object for the superpca
## routine. assume that comp.table and x.hs have the same rows. maps
## them to a list of x,y data frames.
SuperpcReady <- function(comp.table, x.hs){
  specmatrix <- t(x.hs[[]])
  pullfunc <- function(comp.column){list(x = specmatrix, y = comp.column)}
  apply(comp.table,2,pullfunc)
}

## this will cough out predictions for a single response variable,
## from properly formatted test and training data with the same
## y-variable targetted. this is probably going to be our
## workhorse/mapped routine...
RunSuperPc <- function(data.train,data.test,thold,n.comps){
  ## compute scores for each feature
  train.obj <- superpc.train(data.train, type="regression")

  ## compute principal component projection of de-noised features
  ## above a certain threshold, and... we actually need the new data
  ## here! see the implementation... basically, they select features
  ## in data.test and project it in this function. I wonder why?
  fit.object <- superpc.predict(train.obj, data.train, data.test, threshold=thold, n.components=n.comps, prediction.type="continuous")

  ## cough out a predictor
  list(pred=superpc.fit.to.outcome(train.obj,data.test, fit.object$v.pred),fit=fit.object)
}


## Recommended thold: 8, recommended pc's 1
## "383.25"
## "383.3"
## "383.35"
## "467.8"
## "467.85"
## "467.9"
## "467.95"
## "468"
## "468.4"
## "810.75"

## train.oj.k20 <- superpc.predict(trained.k20,train.spc$K2O,test.spc$K2O,t)
