library(glmnet)

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

TestLassoModel <- function(lasso.cv, comps, x.hs){
  pred.vector <- predict(lasso.cv, newx=x.hs@data$spc, s="lambda.1se")
  mean((comps-pred.vector)^2)
}

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

oneSeLars <- function(x.lars.cv){
  min.idx <- which.min(x.lars.cv$cv)
  in.one.se <- x.lars.cv$cv - x.lars.cv$cv[min.idx] < x.lars.cv$cv.error[min.idx]
  which(in.one.se)
  
}
library(glmnet)

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

TestLassoModel <- function(lasso.cv, comps, x.hs){
  pred.vector <- predict(lasso.cv, newx=x.hs@data$spc, s="lambda.1se")
  mean((comps-pred.vector)^2)
}

MakeLarsModel <- function(element, x.hs){
  lars(x.hs[[]], x.hs@data[[element]], normalize = FALSE,
       use.Gram = FALSE, max.steps = 600, trace=TRUE) 
}

ValidateLarsModel <- function(element, x.hs){
  cv.lars(x.hs[[]], x.hs@data[[element]], mode="step", normalize = FALSE,
       use.Gram = FALSE, max.steps = 600, trace=TRUE) 
}

LarsOneSE <- function(x.lars.cv){
  min.error <- min(x.lars.cv$cv)
  min.idx <- which.min(x.lars.cv$cv)
  min.se <- x.lars.cv$cv.error[min.idx]


  ## consider multiplying min.se by 1/2
  within.range <- abs(x.lars.cv$cv - min.error) == 0

  onese.idx <- which(within.range)[1]

  list(idx= onese.idx, error= x.lars.cv$cv[onese.idx],
       stderror=x.lars.cv$cv.error[onese.idx])

}

LarsCoefs <- function(x.lars, step){
  Filter(function(x){x != 0}, coef(x.lars)[step,])
}

EltPresent <- function(lines.db, elt.name, wl, tol){
  regional.lines <- LineSearch(lines.db, wl, tol)
  elements <- as.character(regional.lines$V1)
  matches <- grepl(elt.name, elements)
  print(is.data.frame(regional.lines))
  elt.subset <- regional.lines[matches,]
  
  if (!empty(elt.subset))
      elt.subset$guesswl <- wl
  
  elt.subset
}
## use nrow to "count" the number of rows in the return value from
## Eltpresent

NegNhoods <- function(tol, lines.db, x.lars, step){
  neg.coefs <- Filter(function(x){x < 0},LarsCoefs(x.lars, step))
  neg.coefs.wl <- as.numeric(names(neg.coefs))
  print(neg.coefs.wl)

  neighborhoods <- Map(function(wl){LineSearch(lines.db, wl, tol)}, neg.coefs.wl) 
  Reduce(rbind, neighborhoods)  
}

LineDiscoveryRate <- function(tol, lines.db, x.lars, step, element.name){
  pos.coefs <- Filter(function(x){x > 0},LarsCoefs(x.lars, step))
  pos.coefs.wl <- as.numeric(names(pos.coefs))
  print(pos.coefs.wl)

  matched.lines <- Map(function(wl){EltPresent(lines.db, element.name, wl, tol)}, pos.coefs.wl)
  print(matched.lines)

  neighborhoods <- Map(function(wl){LineSearch(lines.db, wl, tol)}, pos.coefs.wl) 
  
  discovery.rate <- Reduce(`+`,(Map(function(x){if (nrow(x) >= 1) 1 else 0}, matched.lines)))/length(matched.lines)

  list(matches=Reduce(rbind, matched.lines), ldr=discovery.rate,
       neighborhoods=Reduce(rbind, neighborhoods))
  
}

DumpLarsBench <- function(eltname, lars.model, lars.cv, path, lines.db, tol,
                          internal.eltname, test.hs){
  dir.path <- file.path(path, eltname)
  dir.create(dir.path)

  model.choice <- LarsOneSE(lars.cv)
  line.discoveries <- LineDiscoveryRate(tol, lines.db, lars.model,
                                        model.choice$idx, eltname)

  outputs <- list(step=model.choice$idx, guesses=line.discoveries$matches,
                  nhoods=line.discoveries$neighborhoods, train.error=model.choice$error,
                  train.cv.error=model.choice$stderror, path=dir.path,
                  discovery.rate=line.discoveries$ldr, coefs=LarsCoefs(lars.model, model.choice$idx))

  validation.errors <- as.data.frame(list(mse=outputs$train.error, stderr=outputs$train.cv.error))

  neg.nhoods <- NegNhoods(tol, lines.db, lars.model, outputs$step)
  
  write.csv(outputs$guesses, file=file.path(dir.path, "line-guesses.csv"))
  write.csv(outputs$nhoods, file=file.path(dir.path, "pos-nhoods.csv"))
  write.csv(outputs$coefs, file=file.path(dir.path, "lars-coefs.csv"))
  write.csv(validation.errors, file=file.path(dir.path, "val-errors.csv"))
  write.csv(neg.nhoods, file=file.path(dir.path, "neg-nhoods.csv"))

  ldr <- file(file.path(dir.path, "ldr.txt"), "w")  # open an output file connection
  cat(outputs$discovery.rate,"\n", file=ldr)
  close(ldr)

  nsteps <- file(file.path(dir.path, "nsteps.txt"), "w")  # open an output file connection
  cat(outputs$step,"\n", file=nsteps)
  close(nsteps)

  test.results <- LarsTest(lars.model, internal.eltname, outputs$step, test.hs)

  write.csv(test.results$preds, file=file.path(dir.path, "heldout-preds.csv"))
  
  test.mse <- file(file.path(dir.path, "test-mse.txt"), "w")  # open an output file connection
  cat(test.results$mse,"\n", file=test.mse)
  close(test.mse)

  outputs
}

LarsTest <- function(lars.model, elt.name, step, x.hs){
  preds <- predict(lars.model, s=step, x.hs[[]])

  mse <- mean((preds$fit - x.hs@data[[elt.name]])^2)

  list(preds=preds$fit, mse=mse)

}

## the most horrible function call in the history of the world:

## mapply(DumpLarsBench, ext.elt.names, lars.train.models, lars.train.validations, list("/home/marco/lars-output/onese-output"), list(lines.db), list(.5), int.elt.names, list(cset.test))

int.elt.names <- c("SiO2", "Al2O3", "TiO2", "Fe2O3T",
                   "MgO", "MnO", "CaO", "K2O", "Na2O", "P2O5")

ext.elt.names <- c("Si", "Al", "Ti", "Fe",
                   "Mg", "Mn", "Ca", "K", "Na", "P")
