library(pls)

DumpModelTest <- function(x.pls, test.hs, directory, id.vector){
  dir.create(directory)
  chosen.ncomp <- PlsCompGuess(x.pls)$comps
  cat(chosen.ncomp, "\n", file=(file.path(directory, "ncomp.txt")))

  test.id.vector <- as.character(test.hs@data$id)

  ## Predictions (with IDs)
  write.csv(data.frame(id=test.id.vector, predicted=predict(x.pls,newdata=test.hs@data, ncomp=chosen.ncomp)[,,]),
            file.path(directory, "testpredictions.csv"))

  ## RMSEP vs. ncomp curve
  write.csv(RMSEP(x.pls)$val[1,,], file=(file.path(directory, "RMSEP.csv")))

  ## Coefficents of chosen model
  write.csv(coef(x.pls, ncomp=chosen.ncomp), file=(file.path(directory, "coef.csv")))

  ## Spectra (with IDs)
  spc.matrix <- x.pls$x
  rownames(spc.matrix) <- id.vector
  write.csv(spc.matrix, file=file.path(directory, "spectra.csv"))

  ## Loadings (projection coefs for each PC)
  write.csv(t(loadings(x.pls)), file=(file.path(directory, "loadings.csv")))
  
  ## Projections of spectra into PC-space (with IDs)
  score.matrix <- scores(x.pls)
  rownames(score.matrix) <- id.vector
  write.csv(score.matrix, file=(file.path(directory, "scores.csv")))

  ## true Y values (with IDs, just in case)
  y.values.true <- x.pls$y
  rownames(y.values.true) <- id.vector
  write.csv(y.values.true, file=(file.path(directory, "true-y-values.csv")))

  ## Explained variance per-component
  write.csv(attr(scores(x.pls),"explvar"), file=(file.path(directory, "explvar.csv")))
}

## ---------------------------------------------------------------------
## ---------------------- Run PLS on a Data Set ------------------------
## ---------------------------------------------------------------------

## Problem: rmesp call has no way to specify "answers" and
## "predictors". solution: it remembers the formula, so make a
## "prediction-ready" data frame from a compositions data frame and a
## hyperspec object to feed into pls, as matricies to make it all
## happy



MakePlsModels <- function(elements, x.hs){
  sapply(elements, function(y){plsr(as.formula(paste(y," ~ spc")),data=killNA(x.hs,y)@data, ncomp=20, x=TRUE,y=TRUE,validation="CV")},
           USE.NAMES=TRUE,simplify=FALSE)
}


TestPlsModel <- function(x.pls, testdata){
   test.results <- MSEP(x.pls, newdata=testdata@data)
   comps <- PlsCompGuess(x.pls)$comps
   test.results$val[,,comps]
}

PlsSe <- function(x.pls, nc){
  cvraw <- (x.pls$y - x.pls$validation$pred[,,nc])^2
  sd(cvraw)/sqrt(length(x.pls$y))
}

PlsModelPreds <- function(x.pls, testdata){
  comps <- PlsCompGuess(x.pls)$comps
  test.results <- predict(x.pls, newdata=testdata@data, ncomp=comps)
  test.results

}

OneSePlsMsep <- function(x.pls){
  comp.guess <- PlsCompGuess(x.pls)
  error <- PlsError(x.pls)
  list(pls.msep=error$vec[comp.guess$comps],
       pls.se=comp.guess$se[comp.guess$comps])
}

PlsError <- function(x.pls){
  msep.vec <- MSEP(x.pls)$val[1,,2:(x.pls$ncomp+1)]
  ## Why are we using which.min? Shouldn't we be using a comp guess?
  list(min.idx=which.min(msep.vec),
       min.val=min(msep.vec),
       vec=msep.vec)
}


PlsCompGuess <- function(x.pls){
  x.error <- PlsError(x.pls)
  comp.se.list <- sapply(1:x.pls$ncomp,function(x){PlsSe(x.pls,x)})
  comp.within.onese <- (x.error$vec - x.error$min.val) < comp.se.list[x.error$min.idx]
  list(comps=which(comp.within.onese)[1],
       se=comp.se.list,
       within=comp.within.onese)
}

PlsClassifierError <- function(x.hs, element, dlimit, pls.model){
  stripped.x.hs <- killNA(x.hs,element)

  true.dlevels <- stripped.x.hs@data[,element] > dlimit
  predictions <- predict(pls.model,ncomp=PlsCompGuess(pls.model)$comps)[,,]
  guessed.dlevels <- predictions > dlimit
  num.correct <- sum(guessed.dlevels == true.dlevels)
  num.false <- length(true.dlevels) - num.correct 
  num.false/length(true.dlevels)
}

Pls2Error <- function(x.pls){
  errortable <- MSEP(x.pls)$val[1,,2:(x.pls$ncomp+1)]
  apply(errortable,1,function(msep.vec){
  list(min.idx=which.min(msep.vec),
       min.val=min(msep.vec),
       vec=msep.vec)})
}

## IMPORTANT: HOW TO GET TEST RESULTS FOR PLS
## > as.numeric(mapply(TestPlsModel,pls.models,list(cset.test)))

## PLS2 Functions

Pls2Se  <- function(x.pls, nc){
  cvraw <- (x.pls$y - x.pls$validation$pred[,,nc])^2
  sd(cvraw)/sqrt(nrow(cvraw))
}

Pls2CompGuess <- function(x.pls){
  ## I can now trust this thing to supply me with "accurate"
  ## compguesses.
  x.error <- Pls2Error(x.pls)
  comp.se.table <- sapply(1:15,function(x){Pls2Se(x.pls,x)})

  comp.within.table <- mapply(function(error,response){
    comp.within.onese <- (error$vec - error$min.val) < comp.se.table[response,error$min.idx]
    comp.within.onese
  },x.error,1:10)

  list(comps=apply(comp.within.table,2,function(x){which(x)[1]}),
       se=comp.se.table,
       within=comp.within.table)
}

Pls2CvEstimate <- function(x.pls){
  guess <- Pls2CompGuess(x.pls)
}

TakeIndicies <- function(matrix,is,js){
  mapply(function(i,j){matrix[,i,j]},is,js)
}

#lapply(lapply(lasso.models,LassoCoef),file="coef.csv"),function(x){write.csv(x,file=""}

## PLS error bands...

## input: pls model with some kind of validation

## output: for each number of components, the standard error of the
## residuals

## To feed a PLS object to these functions, it MUST include a
## "validation" step.



## "val" includes from the intercept on up. we exclude this, to make
## the minError indicies match the se indicies. Also note that we are
## using the unadjusted CV here.




## > which(RMSEP(new.pls.models[[1]])$val[1,,]-min.error:2.425207 < min.error.std.error:0.09618139)[1]


## find recommended number of comps per pls model
#sapply(lapply(new.pls.models,PlsCompGuess),function(x){x$comps}) 

## this will pull out a per-element validated error estimate table for pls 1
## > mapply(function(x.pls,compguess){PlsError(x.pls)$vec[compguess]},new.pls.models,sapply(lapply(new.pls.models,PlsCompGuess),function(x){x$comps}))

## now we want a per-elment validated se estimate for pls 1
## > lapply(lapply(new.pls.models,PlsCompGuess),function(x){x$se[x$comps]})

## do per-element estimated rmsep and se for the lasso
## > lasso.cv.errortable <- t(sapply(lasso.models,OneSeLassoRmsep))

## test error for the pls models

#> pls.test.results <- lapply(pls.models,function(x){RMSEP(x,newdata=cset.test@data)})
## pull out individual test errors
## > recommended.comps <- sapply(lapply(new.pls.models,PlsCompGuess),function(x){x$comps}) 
## > pls.test.errors <- mapply(function(errors,comps){errors$val[,,comps]},pls.test.results,recommended.comps)

## > lasso.test.errors <- mapply(function(comps,model){TestLassoModel(model,comps,cset.test)},cset.test@data[,6:15],lasso.models)
