library(glmnet)

MakeLassoModels <- function(elements, x.hs){
  sapply(elements, function(y){
    cv.glmnet(killnas(x.hs,y)[[]], killnas(x.hs,y)@data[[y]],nlambda=300)},
         USE.NAMES=TRUE,simplify=FALSE)
}

MakeRidgeModels <- function(elements, x.hs){
  sapply(elements, function(y){
    cv.glmnet(x.hs[[]], x.hs@data[[y]],nlambda=300,alpha=.5)},
         USE.NAMES=TRUE,simplify=FALSE)
}

MakeLassoModels <- function(elements, x.hs){
  sapply(elements, function(y){
    cv.glmnet(killnas(x.hs,y)[[]], killnas(x.hs,y)@data[[y]],nlambda=300)},
         USE.NAMES=TRUE,simplify=FALSE)
}


OneSeLassoMsep <- function(x.lasso.cv){
  min.idx <- which(x.lasso.cv$lambda == x.lasso.cv$lambda.1se)
  print(min.idx)
  list(lasso.msep=x.lasso.cv$cvm[min.idx],
       lasso.se=x.lasso.cv$cvsd[min.idx])
}

OneSeLassoMsep <- function(x.lasso.cv){
  min.idx <- which(x.lasso.cv$lambda == x.lasso.cv$lambda.1se)
  print(min.idx)
  list(lasso.msep=x.lasso.cv$cvm[min.idx],lasso.se=x.lasso.cv$cvsd[min.idx])
}

OneSeLassoMsep <- function(x.lasso.cv){
  min.idx <- which(x.lasso.cv$lambda == x.lasso.cv$lambda.1se)
  print(min.idx)
  list(lasso.msep=x.lasso.cv$cvm[min.idx],
       lasso.se=x.lasso.cv$cvsd[min.idx])
}


TestLassoModel <- function(lasso.cv, comps, x.hs){
  pred.vector <- predict(lasso.cv, newx=x.hs@data$spc, s="lambda.1se")
  mean((comps-pred.vector)^2)
}

TestLassoModel <- function(lasso.cv, comps, x.hs){
  pred.vector <- predict(lasso.cv, newx=x.hs@data$spc, s="lambda.1se")
  mean((comps-pred.vector)^2)
}

TestLassoModel <- function(lasso.cv, comps, x.hs){
  pred.vector <- predict(lasso.cv, newx=x.hs@data$spc, s="lambda.1se")
  mean((comps-pred.vector)^2)
}


LassoModelPreds <- function(lasso.cv, comps, x.hs){
  predict(lasso.cv, newx=x.hs@data$spc, s="lambda.1se")
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

library(glmnet)



