SnipAround <- function(wl, offset, x.hs){
  left.snip <<- wl - offset
  right.snip <<- wl + offset
  cut.hs <- x.hs[,, left.snip ~ right.snip]
  cut.hs
}


LibraryShootout <- function(x.hs, elt.measured.file, elt.region.file, x.comps, element){

  elt.measured <- read.csv(elt.measured.file,header=FALSE)$V2
  elt.regions <- read.csv(elt.region.file)$wl

  x.snipped.hs <- bind(direction='c', mapply(SnipAround, elt.regions, .5, list(x.hs)))
  x.measured.hs <- CutToMeasured(x.snipped.hs, elt.measured)

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
       measured.hs=x.measured.hs,
       element=element)
}


WriteShootout <- function(measured.ncomp, detectable.ncomp, x.shootout, directory, title){
  measured.filename <- paste(x.shootout$element, "measured", title, "all-pred.csv", sep="-")
  comparison.filename <- paste(x.shootout$element, "comparison", title, "pred.csv", sep="-")
  classifier.filename <- paste(x.shootout$element, "classifier", title, "pred.csv", sep="-")
  rmsep.filename <- paste(x.shootout$element, "rmsep", title, "errors.csv", sep="-")

  write.csv(file = paste(directory, measured.filename, sep="/"),
            PredsAtNcomp(x.shootout$measured.pls, measured.ncomp))

  write.csv(file = paste(directory, comparison.filename, sep="/"),
            SideBySide(x.shootout$measured.pls, measured.ncomp,
                       x.shootout$detectable.pls, detectable.ncomp,
                       x.shootout$detectables))

  write.csv(file = paste(directory, classifier.filename, sep="/"),
            x.shootout$detectables)

  write.csv(file = paste(directory, rmsep.filename, sep="/"),
            list(detectable=RMSEP(x.shootout$detectable.pls)$val[1,,],
                 measured=RMSEP(x.shootout$measured.pls)$val[1,,]))

}
