library("kernlab")

TrainSvmModels <- function(elements, x.hs){
  sapply(elements,
         function(y){ksvm(as.formula(paste(y," ~ spc")), data=x.hs@data,
                          kernel="anovadot",cross=10,type="nu-svc")})
  
}

CutToMeasured <- function(x.hs, measured){
  ## Measured.vector has na for missing measurement, 1 for present, 0
  ## for not present
  x.hs <- x.hs[1:length(measured),]
  missing <- measured == "na"
  x.hs <- x.hs[!missing,]
  x.hs$presence <- measured[!missing]
  x.hs
}

PrepforSVMClassification <- function(x.hs){
  x.df <- data.frame(x.hs[[]])
  x.df$presence <- x.hs$presence
  x.df
}

RunComparison <- function(x.hs, x.measured, x.comps, element){
  
  x.measured.hs <- CutToMeasured(x.hs, x.measured) 

  x.comps <- x.comps[element]
  
  x.measured.comps <- x.comps[x.hs$id %in% x.measured.hs$id,]

  x.svm.df <- PrepforSVMClassification(x.measured.hs)

  elt.presence.model <- ksvm(presence ~ ., data = x.svm.df)

  x.fitted.detectable <- as.logical(as.numeric(predict(elt.presence.model))-1)

  x.detectable.comps <- x.measured.comps[x.fitted.detectable]
  x.detectable.hs <- x.measured.hs[x.fitted.detectable]

  detectable.pls1 <- TrainPLSModel(x.detectable.hs, x.detectable.comps)

  measured.pls1 <- TrainPLSModel(x.measured.hs, x.measured.comps)

  list(detectable.pls=detectable.pls1,
       measured.pls=measured.pls1,
       detectables=x.fitted.detectable,
       detectable.svm=elt.presence.model,
       measured.hs=x.measured.hs)
  
}

## seriously just manually inspect the RMSEP curve for first local
## minima: RMSEP(comparison$blah)$val[1,,]

PredsAtNcomp <- function(x.pls,ncomp){
 predict(x.pls)[,,ncomp+1] 
}

SideBySide <- function(measured.pls, measured.ncomp, detectable.pls, detectable.ncomp, detectables){

  list(measured=PredsAtNcomp(measured.pls, measured.ncomp)[detectables],detectable=PredsAtNcomp(detectable.pls, detectable.ncomp))
  
}
