library(hyperSpec)
library(Peaks)

## Functions that actually act on spectra, or lists of
## spectra. Private to this module. External code should only call
## Pipeline.whatever functions.

averageSpots <- function(x.hs){
  print("Averaging spots ")
  aggregate(x.hs, by=x.hs$id, mean)
}

IntensityNormalize <- function(x.hs){
  print("Normalizing rows by intensity")
  sums <- apply(x.hs,1,sum)
  sweep(x.hs,1,sums,"/")
}

AmplitudeMult <- function(x.hs.list, amp.factor){
  lapply(x.hs.list,function(y){apply(y,1,function(z){z*amp.factor})})
}

transformWLsBy <- function(x.hs, wl.transformation){
  wl.indicies <- 1:nwl(x.hs)
  new.wl <-  wl.transformation(wl.indicies)
  wl(x.hs) <- list(wl = new.wl, label = "I/a.u")
  x.hs
}

## Acts on a vector
BaselineFit <- function(x){
  SpectrumBackground(x,decreasing=FALSE, compton=TRUE, window="11",
                     order="6", smoothing=FALSE, iterations=100)
}

RegionalHSList <- function(hs.list){
  list(UV = bind(direction='r',Filter(function(w){w$region=="UV"},hs.list)),
       VIS = bind(direction='r',Filter(function(w){w$region=="VIS"},hs.list)),
       VNIR = bind(direction='r',Filter(function(w){w$region=="VNIR"},hs.list)))
}

SortByRegion <- function(x.hs.list){
  uv.hs.list <- x.hs.list[sapply(x.hs.list,function(x){x@data$region=="UV"})]
  vis.hs.list <- x.hs.list[sapply(x.hs.list,function(x){x@data$region=="VIS"})]
  vnir.hs.list <- x.hs.list[sapply(x.hs.list,function(x){x@data$region=="VNIR"})]
  list(uv=AlphabeticHyperspec(rbind.hyperSpec(uv.hs.list)),
       vis=AlphabeticHyperspec(rbind.hyperSpec(vis.hs.list)),
       vnir=AlphabeticHyperspec(rbind.hyperSpec(vnir.hs.list)))
}

LIBSRegionalHS <- function(hs.list){
  ## sorts the output list by region. suitable for LIBS cset-type
  ## datasets.
  list(UV   = Filter(function(w){w$region=="UV"},hs.list),
       VIS  = Filter(function(w){w$region=="VIS"},hs.list),
       VNIR = Filter(function(w){w$region=="VNIR"},hs.list))
}

## --------------------------------------------------
## -----------Wavelength Standardization-------------
## --------------------------------------------------

## Wavelength axes that we like are noted here. (century set)
CsetUVAxis <- function(x.hs){spc.loess(x.hs,seq(225,325,.05),span=.005)}
CsetVISAxis <- function(x.hs){spc.loess(x.hs,seq(383,470,.05),span=.005)}
CsetVNIRAxis <- function(x.hs){spc.loess(x.hs,seq(495,925,.15),span=.003)}
cset.wl.stdz.funcs <- list(CsetUVAxis,CsetVISAxis,CsetVNIRAxis)
## --------------------------------------------------


## These pipelines make the assumption that a spectrum can be broken into
## three regions, UV, VIS, and VNIR.

Pipeline.BsubAvgSpots <- function(x.hs.list, wl.std.funcs, wl.cal.funcs){
  ## The reason this operates on a list is entirely due to wavelength
  ## standardization; the rest of the stuff could be done on a non
  ## per-region basis. Just call a RegionalHS on the output of
  ## ReadDirectory. Do this directly.
  x.byregion <- RegionalHSList(x.hs.list)
  
  cal.hs.list <- mapply(transformWLsBy, x.byregion, wl.cal.funcs)
  
  ## do.call needs a list of arguments to construct a function call!
  std.hs.list <- mapply(do.call, wl.std.funcs, lapply(cal.hs.list,list))

  bline.hs.list <- lapply(std.hs.list, function(x){apply(x,1,BaselineFit)})
  sub.hs.list <- mapply("-", std.hs.list, bline.hs.list)
  avg.hs.list <- lapply(sub.hs.list, averageSpots)

  list(spec=avg.hs.list, baselines=bline.hs.list)
}

Pipeline.Bsub <- function(x.hs.list, wl.std.funcs, wl.cal.funcs){
  ## The reason this operates on a list is entirely due to wavelength
  ## standardization; the rest of the stuff could be done on a non
  ## per-region basis. Just call a RegionalHS on the output of
  ## ReadDirectory.
  x.byregion <- RegionalHSList(x.hs.list)
  
  cal.hs.list <- mapply(transformWLsBy, x.byregion, wl.cal.funcs)
  
  ## do.call needs a list of arguments to construct a function call!
  std.hs.list <- mapply(do.call, wl.std.funcs, lapply(cal.hs.list,list))

  bline.hs.list <- lapply(std.hs.list, function(x){apply(x,1,BaselineFit)})
  sub.hs.list <- mapply("-", std.hs.list, bline.hs.list)

  list(spec=sub.hs.list, baselines=bline.hs.list)
}

## steps (sample):
## 1. cset.readout <- ReadDirectory(cset.data.dir, J0Readerr)
## 2. cset.pipeout <- Pipeline.BsubAvgSpots(cset.readout, cset.wl.stdz.funcs, cset.wl.cal.funcs)
## 3. cset.hs <- cbind.hyperSpec(cset.pipeout$spec)
## > cset.hs <- AlphabeticHyperspec(cset.hs)
## > cset.hs<- cset.hs[1:100,]
## > cset.hs@data <- cbind(cset.hs@data,cset.major.comps)
