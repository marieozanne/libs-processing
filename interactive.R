## Here be dragons. Or, functions that I have deemed suitable only for
## exploring and displaying the data, not to be directly involved with
## preprocessing or whatnot.

PlotFittedSubbed <- function(original.hs,baseline.hs){
  plot(original.hs)
  plot(baseline.hs,add=TRUE,col=3)
  plot(original.hs-baseline.hs,add=TRUE,col=2)
}

## This function is for display and interactive use only, as it
## operates directly on the slots of a notional hyperspec object

## Baseline Subtractor... one of these for each region
## maps a SINGLE ROW hyperspec object to a hyperspec object with the
## fitted baseline subtracted, CANNOT be called on multiple ROWS.doofus.
BaseLineSubtractorONEROW <- function(x.hs,sub.args){
  print("Subtracting Baseline")
  print(str(x.hs))
  sub.args$y <- x.hs[[]]
  ##sub.args$y <- as.vector(x.hs)
  bline.vector <- do.call(SpectrumBackground,sub.args)
  bl.hs <- new("hyperSpec",spc=bline.vector,wavelength=x.hs@wavelength,label=x.hs@label)
  all.data <- list(original=x.hs,baseline=bl.hs)
  all.data
  #x.hs-bl.hs
}
## example of usage:
## cset.basalts.hs.list <- lapply(cset.hs.list,function(x){SubsetByCharVector(x,sample.groups[,3])})
SubsetByCharVector <- function(x.hs,char.vec){
  samples <- sapply(x.hs$id,function(x){x %in% char.vec})
  split(x.hs,as.factor(samples))$`TRUE`
}


## Quickly get cset bound into one hs thing

