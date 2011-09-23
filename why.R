MakeLassoModels <- function(elements, x.hs){
  sapply(elements, function(y){
    cv.glmnet(x.hs[[]], x.hs@data[[y]],nlambda=300)},
         USE.NAMES=TRUE,simplify=FALSE)
}

MakeRidgeModels <- function(elements, x.hs){
  sapply(elements, function(y){
    cv.glmnet(x.hs[[]], x.hs@data[[y]],nlambda=300,alpha=.5)},
         USE.NAMES=TRUE,simplify=FALSE)
}

TestLassoModel <- function(lasso.cv, comps, x.hs){
  pred.vector <- predict(lasso.cv, newx=x.hs@data$spc, s="lambda.1se")
  mean((comps-pred.vector)^2)
}

LassoModelPreds <- function(lasso.cv, comps, x.hs){
  predict(lasso.cv, newx=x.hs@data$spc, s="lambda.1se")
}

OneSeLassoMsep <- function(x.lasso.cv){
  min.idx <- which(x.lasso.cv$lambda == x.lasso.cv$lambda.1se)
  print(min.idx)
  list(lasso.msep=x.lasso.cv$cvm[min.idx],lasso.se=x.lasso.cv$cvsd[min.idx])
}

LassoCoef <- function(x.lasso.cv){
  min.idx <- which(x.lasso.cv$lambda == x.lasso.cv$lambda.1se)
  coef.idx.list  <- which(x.lasso.cv$glmnet.fit$beta[,min.idx]!=0)
  sort(x.lasso.cv$glmnet.fit$beta[coef.idx.list,min.idx],decreasing=TRUE)
}

ExportLassoCoef <- function(lasso.coef.list,elements){
  mapply(function(co.list,elt){write.csv(co.list,file=paste(elt,".csv",sep=""))},
         lasso.coef.list,
         elements)
}

TestPlsModel <- function(x.pls, testdata){
   test.results <- MSEP(x.pls, newdata=testdata@data)
   comps <- PlsCompGuess(x.pls)$comps
   test.results$val[,,comps]
}

PlsModelPreds <- function(x.pls, testdata){
  comps <- PlsCompGuess(x.pls)$comps
  test.results <- predict(x.pls, newdata=testdata@data, ncomp=comps)
  test.results

}

OneSePlsMsep <- function(x.pls){
  comp.guess <- PlsCompGuess(x.pls)
  error <- PlsError(x.pls)
  list(pls.msep=error$vec[comp.guess$comps],pls.se=comp.guess$se[comp.guess$comps])
}

#lapply(lapply(lasso.models,LassoCoef),file="coef.csv"),function(x){write.csv(x,file=""}

## PLS error bands...

## input: pls model with some kind of validation

## output: for each number of components, the standard error of the
## residuals

## To feed a PLS object to these functions, it MUST include a
## "validation" step.


BetterPlsSe <- function(x.pls,nc){
  ## ## squared loss function
   cvraw <- (x.pls$y - x.pls$validation$pred[,,nc])^2
  ## ## mean squared error
  ## cvm <- mean(cvraw)

  ## sqrt(mean((cvraw-cvm)^2)/(length(x.pls$y)-1))
  sd(cvraw)/sqrt(length(x.pls$y))
}

## "val" includes from the intercept on up. we exclude this, to make
## the minError indicies match the se indicies. Also note that we are
## using the unadjusted CV here.

PlsError <- function(x.pls){
  msep.vec <- MSEP(x.pls)$val[1,,2:(x.pls$ncomp+1)]
  list(min.idx=which.min(msep.vec),
       min.val=min(msep.vec),
       vec=msep.vec)
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

## > which(RMSEP(new.pls.models[[1]])$val[1,,]-min.error:2.425207 < min.error.std.error:0.09618139)[1]

PlsCompGuess <- function(x.pls){
  x.error <- PlsError(x.pls)
  comp.se.list <- sapply(1:15,function(x){BetterPlsSe(x.pls,x)})
  comp.within.onese <- (x.error$vec - x.error$min.val) < comp.se.list[x.error$min.idx]
  list(comps=which(comp.within.onese)[1],
       se=comp.se.list,
       within=comp.within.onese)
}

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
