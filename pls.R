<<<<<<< /home/marco/Dropbox/all-code-versions/code/fresh-start/pls.R
library(pls)

## Modelling and Diagnostic Functions for PLS1
||||||| /home/marco/Dropbox/all-code-versions/code/msl-stats/src/core/pls.R
## ---------------------------------------------------------------------
## ---------------------- Run PLS on a Data Set ------------------------
## ---------------------------------------------------------------------

## Problem: rmesp call has no way to specify "answers" and
## "predictors". solution: it remembers the formula, so make a
## "prediction-ready" data frame from a compositions data frame and a
## hyperspec object to feed into pls, as matricies to make it all
## happy
=======
library(pls)
>>>>>>> /home/marco/Dropbox/all-code-versions/old-code-collection/fresh-start/pls.R

<<<<<<< /home/marco/Dropbox/all-code-versions/code/fresh-start/pls.R
MakePlsModels <- function(elements, x.hs){
  sapply(elements, function(y){plsr(as.formula(paste(y," ~ spc")),data=killNA(x.hs,y)@data, ncomp=20, x=TRUE,y=TRUE,validation="CV")},
           USE.NAMES=TRUE,simplify=FALSE)
}

TestPlsModel <- function(x.pls, testdata){
   test.results <- MSEP(x.pls, newdata=testdata@data)
   comps <- PlsCompGuess(x.pls)$comps
   test.results$val[,,comps]
}

PlsSe <- function(x.pls,nc){
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

## PlsDumpModel <- function(x.pls,directory)

PlsClassifierError <- function(x.hs, element, dlimit, pls.model){
  stripped.x.hs <- killNA(x.hs,element)

  true.dlevels <- stripped.x.hs@data[,element] > dlimit
  predictions <- predict(pls.model,ncomp=PlsCompGuess(pls.model)$comps)[,,]
  guessed.dlevels <- predictions > dlimit
  num.correct <- sum(guessed.dlevels == true.dlevels)
  num.false <- length(true.dlevels) - num.correct 
  num.false/length(true.dlevels)
}

DumpModel <- function(x.pls, id.vector, directory){
  chosen.ncomp <- PlsCompGuess(x.pls)$comps
  cat(chosen.ncomp, "\n", file=(file.path(directory, "ncomp.txt")))

  ## Predictions (with IDs)
  write.csv(data.frame(id=id.vector, predicted=predict(x.pls, ncomp=chosen.ncomp)[,,]),
            file.path(directory, "predictions.csv"))

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

||||||| /home/marco/Dropbox/all-code-versions/code/msl-stats/src/core/pls.R
PlsReady <- function(x.hs,x.comps){
  list(comps=as.matrix(x.comps),spectra=x.hs[[]])
}

HackyWithDistancePlsReady <- function(x.matrix,x.comps){
  list(comps=as.matrix(x.comps),spectra=x.matrix)
}

## just broke pls1 support.
SplitComps <- function(x.comps.table, x.hs){
  apply(x.comps.table, 2, function(j) {list(comps=j, hs=x.hs)})
}

SuperPCReady <- function(x.hs,x.comps){
  list(y=as.matrix(x.comps),x=t(x.hs[[]]))
}


## Training PLS Models
## --------------------------------------------------

## pls1 is broken, fix later: make splitcomps return two lists, mapply
## TrainPLSModel into them.
TrainPLS1Model <- function(x.hs,x.comps){
  submodels.traindata <- SplitComps(x.comps,x.hs)
  lapply(submodels.traindata,TrainPLSModel)}

TrainPLS2Model <- function(x.hs,x.comps){
  TrainPLSModel(x.hs,x.comps)}

TrainPLSModel <- function(x.hs,x.comps){
  model.plsready <- PlsReady(x.hs,x.comps)
  plsr(comps ~ spectra, ncomp = 10, data = model.plsready, validation="LOO")}

## Internal Validation Functions
## --------------------------------------------------

ival.pls1.fe <- function(x.pls1.submodels){
  ## RMSEP object from > trained.pls1.cset.and.phy.internalval <-
  ## lapply(trained.pls1.cset,RMSEP) type object list, simplified and
  ## turned into a nice flat data frame in the shape that we actually
  ## want.
  x.pls1.rmsep.list <- lapply(x.pls1.submodels,RMSEP)
  t(as.data.frame(lapply(x.pls1.rmsep.list,function(x){x$val[1,,]})))
}

ival.pls2.fe <- function(x.pls2.model){
   RMSEP(x.pls2.model)$val[1,,]
}

## Unseen data test functions
## --------------------------------------------------

TestPLS2Model <- function(model,test.hs,test.comps){
  test.plsready <- PlsReady(test.hs,test.comps)
  RMSEP(model, newdata=test.plsready)$val[1,,]
}

TestPLS1Model <- function(model,test.hs,test.comps){
  split.comp.list <- SplitComps(test.comps,test.hs)
  x.pls1.rmsep.list <- mapply(function(submodel, y){RMSEP(submodel, newdata=PlsReady(y$hs, y$comps))}, model, split.comp.list, SIMPLIFY = FALSE)
  t(as.data.frame(lapply(x.pls1.rmsep.list,function(x){x$val[1,,]})))
}


## Model Selection Functions
## --------------------------------------------------
firstLocalMin <- function(sequence){
for(i in 3:length(sequence)){
  #print(sequence[i])
  #print(i)
  if((sequence[i-1] > sequence[i]) & (sequence[i] < sequence[i+1])){
    print(c(i,"first minima"))
    print(c(i-1,"number of components"))
    return(i)
   }
 } 
}

GlobalMin <- function(sequence){
  ## The minus one is because components live in the slot one below
  ## their number
  which.min(sequence)
}

pls.GlobalMin <- function(x.flaterrortable){
  apply(x.flaterrortable,1,GlobalMin)
}

pls.firstLocalMin <- function(x.flaterrortable){
  apply(x.flaterrortable,1,firstLocalMin)
}

## Model Evaluation Functions
## --------------------------------------------------

ModelPred <- function(x.pls2, y.hs, model.select.fcn){
  x.val.fe <- ival.pls2.fe(x.pls2)
  x.predictions.box <- PredwithPLS2Model(x.pls2, y.hs)
  perelement.model.selections <- model.select.fcn(x.val.flaterrors)
}

PredwithPLS2Model <- function(x.pls2, y.hs){
  all.preds <- predict(x.pls2, newdata=y.hs[[]])
  all.preds
  
}

EvalModel <- function(x.pls2, y.hs, y.comps, model.select.fcn){
  x.val.fe <- ival.pls2.fe(x.pls2)
  x.test.fe <- TestPLS2Model(x.pls2, y.hs, y.comps)
  EvaluateModel(x.val.fe, model.select.fcn, x.test.fe)
}

EvaluateModel <- function(val.flaterrors, model.select.fcn, test.flaterrors){
  perelement.model.selections <- model.select.fcn(val.flaterrors)
  testerror.list <- as.data.frame(t(test.flaterrors))
  print(perelement.model.selections)
  mapply(function(x,j){x[j]}, testerror.list, perelement.model.selections)
}

NamesCat <- function(x.list,addition){
  ## utility function: adds a string to the end of each entry in a
  ## character vector.
  sapply(names(x.list),function(x){paste(x,addition,sep=".")},USE.NAMES=FALSE)
}
=======
## Modelling and Diagnostic Functions for PLS1
>>>>>>> /home/marco/Dropbox/all-code-versions/old-code-collection/fresh-start/pls.R

<<<<<<< /home/marco/Dropbox/all-code-versions/code/fresh-start/pls.R
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
||||||| /home/marco/Dropbox/all-code-versions/code/msl-stats/src/core/pls.R

map.test <- function(models, test.fcn, tag, test.hs, test.comps){
  names(models) <- NamesCat(models, tag)
  mapply(function(x){test.fcn(x, test.hs, test.comps)}, models, USE.NAMES=TRUE,SIMPLIFY=FALSE)
}

map.ival <- function(models, val.fcn){
  names(models) <- NamesCat(models, "ival")
  mapply(val.fcn,models,USE.NAMES=TRUE,SIMPLIFY=FALSE)
}

DirtyAbsoluteError <- function(lonely.spectrums, only.composition, model, nc){
  test.points <- lapply(lonely.spectrums, function(x.hs){PlsReady(x.hs,only.composition)})
  errors.list <- lapply(test.points, function(point){point$comps-predict(model,ncomp=nc,newdata=point)[,,]})
  errors.table <- do.call(rbind,errors.list)
  rownames(errors.table) <- names(errors.list)
  errors.table
}













## The Cornfield,,
## --------------------------------------------------

## map.exval <- function(models, test.fcn, tag){
##   names(models) <- NamesCat(models, tag)
##   mapply(function(x){test.fcn(x, sed.val.hs, sed.val.major.comps)}, models, USE.NAMES=TRUE,SIMPLIFY=FALSE)
## }

## map.extest <- function(models, test.fcn, tag){
##   names(models) <- NamesCat(models, tag)
##   mapply(function(x){test.fcn(x, sed.hs.test, sed.major.comps)}, models, USE.NAMES=TRUE,SIMPLIFY=FALSE)
## }

## map.testmodels <- function(models, test.fcn){
##   names(models) <- NamesCat(models, "tested")
##   mapply(function(x){test.fcn(x, sed.test.hs, sed.test.major.hs)}, models, USE.NAMES=TRUE, SIMPLIFY=FALSE)}
##> as.data.frame(SelectModels(pls1.valresults,list(global=pls.GlobalMin,fstloc=pls.firstLocalMin)))



OneSigmaPredictionTest <- function(model,predictors,pc.count,compositions){
  ## model is a PLS model
  ## newdata MUST BE a hyperspec object
  ## pc.count is an integer specifying number of pc's to use
  ## compositions is a matrix matching the dimension of
  predictions <- predict(model,newdata=predictors[[]])[,,pc.count]
  list(errors=sd(compositions - predictions),
       results=predictions)
=======
MakePlsModels <- function(elements, x.hs){
  sapply(elements, function(y){plsr(as.formula(paste(y," ~ spc")),data=killNA(x.hs,y)@data, ncomp=20, x=TRUE,y=TRUE,validation="CV")},
           USE.NAMES=TRUE,simplify=FALSE)
>>>>>>> /home/marco/Dropbox/all-code-versions/old-code-collection/fresh-start/pls.R
}

<<<<<<< /home/marco/Dropbox/all-code-versions/code/fresh-start/pls.R
MakeDirectoryNames <- function(base.name, element.names){
  Map(function(base,elt){file.path(base,elt)}, list(base.name), element.names)
  
||||||| /home/marco/Dropbox/all-code-versions/code/msl-stats/src/core/pls.R
OSPredictionTest <- function(model,testset,pc.count){
  # testset is a PlsReady object
  predictions <- predict(model,newdata=testset)[,,pc.count]
  list(oserrors=sd(testset$comps - predictions),
       rmsep=RMSEP(model,ncomp=pc.count,newdata=testset),
       results=predictions)
}


TrainPLSModels <- function(trainset.list){
  ## this was bad because it mixed iteration over a list with my only
  ## way of actually calling the plsr library. talk about your
  ## confusion of abstractions... but it might still be good to have
  ## this around as a wrapper over a sane function?
 trainset.plsready <- lapply(trainset.list,function(x){PlsReady(x$hs,x$comps)}) 
 lapply(trainset.plsready, function(x){plsr(comps ~ spectra, ncomp = 15, data = x, validation="LOO")}) 
}


SelectModels <- function(models.valouts,model.select.fcns){
  lapply(models.valouts, function(model.valouts){
    lapply(model.valouts, function(model.val){
      lapply(model.select.fcns, function(model.select.fcn){model.select.fcn(model.val)})
    })
  })
}
## this is really, REALLY dirty: notice the use of validation code for
## testing. ugh.
DirtyModelTest.pls1 <- function(model,x.hs.test,x.comps.test){
  tester <- make.pls1.evalidator(x.hs.test,x.comps.test)
  tester(model)
}
## I guess lapply this awful thing to each list of by-response pls1
## models according to data set.
DirtyTestResults.pls1 <- function(models,x.hs.test,x.comps.test){
  lapply(models, function(z){DirtyModelTest.pls1(z,x.hs.test,x.comps.test)})
}

GetModelTestResults <- function(model.selections,model.results){
  lapply(model.selections)

}

ValOutput <- function(model.list, val.funks){
  lapply(model.list, function(x){
    lapply(val.funks, function(g){g(x)})
  })  
}

interleave <- function(v1,v2){
  ord1 <- 2*(1:length(v1))-1
  ord2 <- 2*(1:length(v2))
  c(v1,v2)[order(c(ord1,ord2))]}

pls1.errortable.GlobalMin <- function(x.pls1.errortable){
  lapply(pls1.externalval.phy,function(x)(GlobalMin(x$val)))
}

pls2.errortable.GlobalMin <- function(x.pls2.errortable){
  apply(x.pls2.errortable$val[1,,],1,GlobalMin)
}

pls1.errortable.firstLocalMin <- function(x.pls1.errortable){
  lapply(x.pls1.errortable, function(x){firstLocalMin(x$val[1,,])})
}

pls2.errortable.firstLocalMin <- function(x.pls2.errortable){
  apply(x.pls2.errortable$val[1,,],1,firstLocalMin)
}

## Response: SiO2 
## (Intercept)      1 comps      2 comps      3 comps      4 comps      5 comps  
##       3.601        8.999        7.399       11.357       11.989        7.125  
##     6 comps      7 comps      8 comps      9 comps     10 comps     11 comps  
##       6.351        7.007        5.502        6.491        6.554        7.152  
##    12 comps     13 comps     14 comps     15 comps  
##       6.982        6.541        6.784       10.303  
## > nice.pls1.cset.internalval <- sapply(trained.pls1.cset.internalval,function(x){summary(x$val)})
## > nice.pls2.cset.internalval <- summary(t(simple.internel.rmsep.table.pls2))
## > write.csv(nice.pls1.cset.internalval,file="/home/marco/nice.pls1.cset.internalval.csv")
## > write.csv(nice.pls2.cset.internalval,file="/home/marco/nice.pls2.cset.internalval.csv")

## > write.csv2(lapply(pls1.cset.and.phy.externalval,function(x){x$val}),file="/home/marco/pls1.cset.and.phy.externalval.csv")
## > write.csv2(lapply(trained.pls1.cset.and.phy.internalval,function(x){x$val}),file="/home/marco/pls1.cset.and.phy.internalval.csv")
## > 

## How do I get firstLocalMin to run on my tables of RMSEP's?
## pls1: > firstLocalMin(pls1.externalval.phy[[1]]$val[,,])
## pls1: lapply(pls1.externalval.phy, function(x){firstLocalMin(x$val)})
## pls2: > apply(external.val.rmseps$cset$val[1,,],1,firstLocalMin)
## How do I get GlobalMin to run on my tables of RMSEP's?
## pls1: lapply(pls1.externalval.phy,function(x)(GlobalMin(x$val)))
## generating lines of that table.
## > mapply(function(x,y){list(comp=y-1,error=flat.test.results[x,y])},1:10,comp.selections)
## turning pls1 test results into something easier to work with:
## In all functions below, we take the CV, not the adjusted CV. They
## do not differ much.
make.pls1.evalidator <- function(x.hs,x.comps){
  function(z.pls1.model){
    t(as.data.frame(lapply(MultiTestPLSModels(z.pls1.model,x.hs,x.comps),function(x){x$val[1,,]})))
  }
}

eval.pls2 <- function(){}

RMSEPtoFE <- function(rmsep.obj){

  rmsep.obj$val[,,]

=======
TestPlsModel <- function(x.pls, testdata){
   test.results <- MSEP(x.pls, newdata=testdata@data)
   comps <- PlsCompGuess(x.pls)$comps
   test.results$val[,,comps]
}

PlsSe <- function(x.pls,nc){
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

## PlsDumpModel <- function(x.pls,directory)

PlsClassifierError <- function(x.hs, element, dlimit, pls.model){
  stripped.x.hs <- killNA(x.hs,element)

  true.dlevels <- stripped.x.hs@data[,element] > dlimit
  predictions <- predict(pls.model,ncomp=PlsCompGuess(pls.model)$comps)[,,]
  guessed.dlevels <- predictions > dlimit
  num.correct <- sum(guessed.dlevels == true.dlevels)
  num.false <- length(true.dlevels) - num.correct 
  num.false/length(true.dlevels)
>>>>>>> /home/marco/Dropbox/all-code-versions/old-code-collection/fresh-start/pls.R
}


## NEWEST SECTION

  
library(pls)

## Modelling and Diagnostic Functions for PLS1

MakePlsModels <- function(elements, x.hs){
  sapply(elements, function(y){plsr(as.formula(paste(y," ~ spc")),data=killNA(x.hs,y)@data, ncomp=20, x=TRUE,y=TRUE,validation="CV")},
           USE.NAMES=TRUE,simplify=FALSE)
}

TestPlsModel <- function(x.pls, testdata){
   test.results <- MSEP(x.pls, newdata=testdata@data)
   comps <- PlsCompGuess(x.pls)$comps
   test.results$val[,,comps]
}

PlsSe <- function(x.pls,nc){
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

## PlsDumpModel <- function(x.pls,directory)

PlsClassifierError <- function(x.hs, element, dlimit, pls.model){
  stripped.x.hs <- killNA(x.hs,element)

  true.dlevels <- stripped.x.hs@data[,element] > dlimit
  predictions <- predict(pls.model,ncomp=PlsCompGuess(pls.model)$comps)[,,]
  guessed.dlevels <- predictions > dlimit
  num.correct <- sum(guessed.dlevels == true.dlevels)
  num.false <- length(true.dlevels) - num.correct 
  num.false/length(true.dlevels)
}
