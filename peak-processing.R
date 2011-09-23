library("PROcess")

##>  demp <- isPeak(phy.process.in[[1]],plot=TRUE,ratio=.05,zerothrsh=.05,span=41,SoN=.5)
## current parameters for "isPeak" from the "process" library

HsToPROcessInputs <- function(x.hs){
  single.spectrum.objs <- split(x.hs, seq(1, nrow(x.hs)))
  names(single.spectrum.objs) <- x.hs$id
  lapply(single.spectrum.objs, function(row){as.long.df(row)[,1:2]})}

PROcessPkFind <- function(matrix){
  isPeak(matrix,plot=TRUE)
}

PROcessToInternalRep <- function(pro.df, sample.id){
  actual.peaks <- subset(pro.df, peak == TRUE)
  new.df = with(actual.peaks, data.frame(mz, sigmas, smooth, area))
  names(new.df) <- c("mu","sigma","intensity","area")
  new.df$sample <- sample.id
  new.df
}

ExtractPeakAreas <- function(x.hs, hsToPFinputs, peakFinder, toOurPeakRep){
  peakFinder.inputs <- hsToPFinputs(x.hs)
  peakFinder.outputs <- lapply(peakFinder.inputs, peakFinder)
  peaks.OurRep <- do.call(rbind,mapply(toOurPeakRep, peakFinder.outputs, x.hs$id,SIMPLIFY=FALSE))
  peaks.OurRep
}

## Sample Call:
## > cset.allpeaks <- ExtractPeakAreas(cset.straight.bsub.hs, HsToPROcessInputs, PROcessPkFind, PROcessToInternalRep)

## Missing: building a cluster analysis, cutting the cluster, adding
## identifictaion numbers to our internal peak representation

IDPeaks <- function(x.ipks){
  ## precondition: no peak identification info in x.ipks
  distmat.x <- dist(x.ipks$mu)
  fit <- hclust(distmat.x, method="ward")
  peak.ids  <- cutree(fit, h=.01)
  x.ipks$peak.id <- peak.ids
  x.ipks
}

BuildRegressable <- function(x.ipeaks){
  x.ipeaks$sample.number <- as.numeric(as.factor(x.ipeaks$sample))
  regress <-  matrix(data=0, length(unique(x.ipeaks$sample)), length(unique(x.ipeaks$peak.id)))
  mapply(function(s,i,a){regress[s,i] <<- a},x.ipeaks$sample.number,x.ipeaks$peak.id,x.ipeaks$area)
  regress
}

SnipSearch <- function(x.hs, midpoint){
  print(x.hs)
  print(midpoint)
  left.snip <<- midpoint - 6
  right.snip <<- midpoint + 6
  cut.hs <- x.hs[,, left.snip ~ right.snip]
  smoothed.hs<- spc.loess(cut.hs, seq(left.snip,right.snip,.003),span=.05,degree=2,family="gaussian")
  print(smoothed.hs)
  plot(smoothed.hs)
  cut.peaks <- ExtractPeakAreas(smoothed.hs, HsToPROcessInputs, PROcessPkFind, PROcessToInternalRep)
  IDPeaks(cut.peaks)
}
cset.snip <- function(x){SnipSearch(cset.straight.bsub.hs,x)}
