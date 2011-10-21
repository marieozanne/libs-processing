library(lars)
library(R.utils)
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
  ## within.range <- abs(x.lars.cv$cv - min.error) < min.se
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
  
  if (!nrow(elt.subset) == 0)
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

  write.csv(test.results, file=file.path(dir.path, "heldout-preds.csv"))
  
  test.mse <- file(file.path(dir.path, "test-mse.txt"), "w")  # open an output file connection
  cat(test.results$mse,"\n", file=test.mse)
  close(test.mse)

  outputs
}

LarsTest <- function(lars.model, elt.name, step, x.hs){
  preds <- predict(lars.model, s=step, x.hs[[]])

  mse <- mean((preds$fit - x.hs@data[[elt.name]])^2)

  list(preds=preds$fit, mse=mse, id=as.character(x.hs$id))

}

## the most horrible function call in the history of the world:

## mapply(DumpLarsBench, ext.elt.names, lars.train.models, lars.train.validations, list("/home/marco/lars-output/onese-output"), list(lines.db), list(.5), int.elt.names, list(cset.test))

int.elt.names <- c("SiO2", "Al2O3", "TiO2", "Fe2O3T",
                   "MgO", "MnO", "CaO", "K2O", "Na2O", "P2O5")

ext.elt.names <- c("Si", "Al", "Ti", "Fe",
                   "Mg", "Mn", "Ca", "K", "Na", "P")
