library("hyperSpec")
library("pls")
library("Peaks")

## Use this function to collapse a hyperspec object into sample id by
## mean spectrum for this sample, which COULD POSSIBLY have been taken
## at different times. We hope that this smoothes out jitter or
## something.
averageSpots <- function(x.hs){
  print("Averaging spots ")
  aggregate(x.hs, by=x.hs$id, mean)
}

transformWLsBy <- function(x.hs,wl.transformation){
  wl.indicies <- 1:nwl(x.hs)
  new.wl <-  wl.transformation(wl.indicies)
  wl(x.hs) <- list(wl = new.wl, label = "I/a.u")
  x.hs
}

loadDataSet <- function(data.dir,wl.cal.funcs){
  ## Takes a wavelength calibration function list (per-region) of a
  ## data set. This is a property of the data set, and thus, I think,
  ## belongs in this function.
  
  ## New resolution: this thing outputs un-normalized, un-averaged,
  ## per-region lists of samples, one row per file,
  ## wavelength-calibrated. THAT'S ALL.
  spectra.hs.list <- ReadRawDataSet(data.dir, J0Reader)
  mapply(transformWLsBy,spectra.hs.list,wl.cal.funcs)
}

BlineGen <- function(sub.args){
  function(x){
    sub.args$y <- x
    do.call(SpectrumBackground,sub.args)
  }
}

## closest thing to a good baseline subtraction parameters for VNIR
## that I could find, through experimentation etc. declare victory and
## move on!

VNIR.BL <- list(decreasing=FALSE, compton=TRUE, window="11",
                order="6", smoothing=FALSE, iterations=100)

BlineDefault <- BlineGen(VNIR.BL)


BsubPlot <- function(spectra.hs, baselines.hs, index){
  plot(spectra.hs[index])
  plot(baselines.hs[index],add=TRUE,col=2)
#  abline(v=100,col=3)
}

## --------------------------------------------------
## ------------Wavelength Standardization------------
## --------------------------------------------------
WvLenStd <- function(x.hs.list,std.func.list){
  ## Input: list of spectra by region, list of axis
  ## interpolation/standardization functions by region. Obviously, the
  ## ordering on regions must be THE SAME in both of these lists.
  x.hs.list <- lapply(x.hs.list,list)
  ## because do.call needs a LIST of arguments to construct a function
  ## call.
  mapply(do.call,std.func.list,x.hs.list)
}


## Wavelength axes that we like are noted here.
StdUVAxis <- function(x.hs){spc.loess(x.hs,seq(225,325,.05),span=.005)}

StdVISAxis <- function(x.hs){spc.loess(x.hs,seq(383,470,.05),span=.005)}

StdVNIRAxis <- function(x.hs){spc.loess(x.hs,seq(495,925,.15),span=.003)}

wl.stdz.funcs <- list(StdUVAxis,StdVISAxis,StdVNIRAxis)

## Note to self: I thought we averaged before baseline subtraction?
## Check and see if this makes a difference.

Pipeline.OnlyBaseline <- function(x.hs.list){
  s.hs.list <- WvLenStd(x.hs.list,wl.stdz.funcs)
  bline.hs.list <- lapply(s.hs.list, function(x){apply(x,1,BlineDefault)})
  sub.hs.list <- mapply("-",s.hs.list,bline.hs.list)
  f.hs.list <- lapply(sub.hs.list,averageSpots)
  list(spec=f.hs.list,baselines=bline.hs.list)
}

OneHS <- function(x.data.dir,x.wl.cal.funcs,preprocess.funk){
  raw.hs.list <- loadDataSet(x.data.dir,x.wl.cal.funcs)
  x.pipeout <- preprocess.funk(raw.hs.list)
  bind(direction='c',x.pipeout$spec)
}

## OneHS(cset.data.dir, cset.wl.cal.funcs, Pipeline.OnlyBaseline)

## rawReader Functions: Utilities and Specific rawReader Functions

## A rawReader function takes a filename and outputs a single-row
## hyperspec object, preferably with appropriate metadata appended to
## the hyperspec object. When you make these functions, DOCUMENT
## what metadata is included, if any.


## ---------------------------------------------------------------------
## RawReader: J0Reader & Helper Functions
J0Reader <- function(filename){
  print(paste("Reading",filename))
  reader.fcn <- longReaderGen(19,2048)
  file.hs <- reader.fcn(filename)
  file.mdata.list <- filename.to.factorlist(filename)
  ## fix this and generalize list addition to the hyperspec object.
  file.hs$id <- file.mdata.list$id
  file.hs$region <- file.mdata.list$region
  file.hs$spot <- file.mdata.list$spot
  rownames(file.hs[[]]) <- c(file.hs$id)
  file.hs
}

make.mdata.list <- function(datamat){
  sapply(row.names(datamat),filename.to.factorlist,simplify=FALSE,USE.NAMES=TRUE)
}

filename.to.factorlist <- function(filename){
  bare.filename <- tail(strsplit(filename,split="/",fixed=TRUE)[[1]],1)
  split.filename <- strsplit(bare.filename,split="[[:space:]]|[.]")[[1]]
  split.filename <- split.filename[split.filename != ""]
  list(id=paste(split.filename[2],split.filename[1],sep=""),
       region=split.filename[3],
       spot=split.filename[4])
}
