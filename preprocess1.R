<<<<<<< /home/marco/Dropbox/all-code-versions/code/msl-stats/src/core/preprocess.R
## Here is where all the preprocessing functions live.

PreProcessforPLS <- function(pipeline.funk, x.hs.list, x.comps){
  done.x.hs.list <- pipeline.funk(x.hs.list)
  x.hs <- bind('c',done.x.hs.list)
  x.plsready <- PlsReady(x.hs,x.comps)
  x.plsready
}


TestMultiplePipelines <- function(funk.list, x.hs.list){
  lapply(funk.list,do.call,list(x.hs.list))
}

## ----------------- Pre-Processing -----------------
## --------------------------------------------------

J0Normalize <- function(x.hs){
  normalizeRows(averageSpots(x.hs))
}

## Use this function to collapse a hyperspec object into sample id by
## mean spectrum for this sample, which COULD POSSIBLY have been taken
## at different times. We hope that this smoothes out jitter or
## something.
=======
library(hyperSpec)
library(pls)
library(Peaks)
## Functions that actually act on spectra
transformWLsBy <- function(x.hs, wl.transformation){
  wl.indicies <- 1:nwl(x.hs)
  new.wl <-  wl.transformation(wl.indicies)
  wl(x.hs) <- list(wl = new.wl, label = "I/a.u")
  x.hs
}

>>>>>>> /home/marco/Dropbox/all-code-versions/old-code-collection/fresh-start/preprocess.R
averageSpots <- function(x.hs){
  print("Averaging spots ")
  aggregate(x.hs, by=x.hs$id, mean)
}

<<<<<<< /home/marco/Dropbox/all-code-versions/code/msl-stats/src/core/preprocess.R
IntensityNormalize <- function(x.hs){
  print("Normalizing rows by intensity")
  sums <- apply(x.hs,1,sum)
  sweep(x.hs,1,sums,"/")
}

transformWLsBy <- function(x.hs,wl.transformation){
  wl.indicies <- 1:nwl(x.hs)
  new.wl <-  wl.transformation(wl.indicies)
  wl(x.hs) <- list(wl = new.wl, label = "I/a.u")
  x.hs
}

=======
>>>>>>> /home/marco/Dropbox/all-code-versions/old-code-collection/fresh-start/preprocess.R
AmplitudeMult <- function(x.hs.list, amp.factor){
  lapply(x.hs.list,function(y){apply(y,1,function(z){z*amp.factor})})
}

<<<<<<< /home/marco/Dropbox/all-code-versions/code/msl-stats/src/core/preprocess.R
#csvToHyperSpec <- 

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

MajorComps <- function(comps.file){
  ## I don't really need this function, it's a wrapper around
  ## read.table ffs.
  ## grab the most used elements from an rtable 
  comps.all.table <- read.table(comps.file)
  comps.all.table[,1:10]
}

=======
>>>>>>> /home/marco/Dropbox/all-code-versions/old-code-collection/fresh-start/preprocess.R
## --------------------------------------------------
## --------------Baseline Subtraction----------------
## --------------------------------------------------
<<<<<<< /home/marco/Dropbox/all-code-versions/code/msl-stats/src/core/preprocess.R
BLSubGen <- function(sub.args){
  function(x.hs){
    print("Subtracting Baseline")
    print(str(x.hs))
    sub.args$y <- x.hs
    ##sub.args$y <- as.vector(x.hs)
    bline.vector <- do.call(SpectrumBackground,sub.args)
    bl.hs <- new("hyperSpec",spc=bline.vector,wavelength=x.hs@wavelength,label=x.hs@label)
    all.data <- list(original=x.hs,baseline=bl.hs)
    ##all.data
    x.hs-bl.hs
  }
}
=======
>>>>>>> /home/marco/Dropbox/all-code-versions/old-code-collection/fresh-start/preprocess.R

<<<<<<< /home/marco/Dropbox/all-code-versions/code/msl-stats/src/core/preprocess.R
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
VIS.BL <- list(decreasing=FALSE, compton=TRUE, window="11",
               order="6", smoothing=FALSE, iterations=100)
UV.BL <- list(decreasing=FALSE, compton=TRUE, window="11",
              order="6", smoothing=FALSE, iterations=100)

BlineDefault <- BlineGen(VNIR.BL)

BsubPlot <- function(spectra.hs, baselines.hs, index){
  plot(spectra.hs[index])
  plot(baselines.hs[index],add=TRUE,col=2)
#  abline(v=100,col=3)
=======
BaselineSubtract <- function(x){
  SpectrumBackground(x,decreasing=FALSE, compton=TRUE, window="11",
                     order="6", smoothing=FALSE, iterations=100)
>>>>>>> /home/marco/Dropbox/all-code-versions/old-code-collection/fresh-start/preprocess.R
}

## --------------------------------------------------
<<<<<<< /home/marco/Dropbox/all-code-versions/code/msl-stats/src/core/preprocess.R
## ------------Wavelength Standardization------------
=======
## -----------Wavelength Standardization-------------
>>>>>>> /home/marco/Dropbox/all-code-versions/old-code-collection/fresh-start/preprocess.R
## --------------------------------------------------
<<<<<<< /home/marco/Dropbox/all-code-versions/code/msl-stats/src/core/preprocess.R
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
=======

## Wavelength axes that we like are noted here. (century set)
CsetUVAxis <- function(x.hs){spc.loess(x.hs,seq(225,325,.05),span=.005)}
CsetVISAxis <- function(x.hs){spc.loess(x.hs,seq(383,470,.05),span=.005)}
CsetVNIRAxis <- function(x.hs){spc.loess(x.hs,seq(495,925,.15),span=.003)}
cset.wl.stdz.funcs <- list(CsetUVAxis,CsetVISAxis,CsetVNIRAxis)
>>>>>>> /home/marco/Dropbox/all-code-versions/old-code-collection/fresh-start/preprocess.R
## --------------------------------------------------

<<<<<<< /home/marco/Dropbox/all-code-versions/code/fresh-start/preprocess.R

Pipeline <- function(x.hs.list, wl.std.funcs, wl.cal.funcs){
  ## The reason this operates on a list is entirely due to wavelength
  ## standardization; the rest of the stuff could be done on a non
  ## per-region basis. Just call a RegionalHS on the output of
  ## ReadDirectory.
  cal.hs.list <- mapply(transformWLsBy, x.hs.list, wl.cal.funcs)
  ## do.call needs a list of arguments to construct a function call!
  std.hs.list <- mapply(do.call, wl.std.funcs, lapply(cal.hs.list,list))
  bline.hs.list <- lapply(std.hs.list, function(x){apply(x,1,BaselineSubtract)})
  sub.hs.list <- mapply("-",std.hs.list,bline.hs.list)
  
  f.hs.list <- lapply(sub.hs.list,averageSpots)
  unsub.hs.list <- lapply(std.hs.list,averageSpots)
 # list(usubspec=std.hs.list, baselines=bline.hs.list, bsubspec=sub.hs.list)
   list(spec=f.hs.list, baselines=bline.hs.list, bsubspec=sub.hs.list)
}

SortByRegion <- function(x.hs.list){
  uv.hs.list <- x.hs.list[sapply(x.hs.list,function(x){x@data$region=="UV"})]
  vis.hs.list <- x.hs.list[sapply(x.hs.list,function(x){x@data$region=="VIS"})]
  vnir.hs.list <- x.hs.list[sapply(x.hs.list,function(x){x@data$region=="VNIR"})]
  list(uv=AlphabeticHyperspec(rbind.hyperSpec(uv.hs.list)),
       vis=AlphabeticHyperspec(rbind.hyperSpec(vis.hs.list)),
       vnir=AlphabeticHyperspec(rbind.hyperSpec(vnir.hs.list)))
}
## steps:
## 1. readdirectory
## 2. sortbyregion
## 3. pipeline 
||||||| /home/marco/Dropbox/all-code-versions/code/msl-stats/src/core/preprocess.R
StdUVAxisVenusCut <- function(x.hs){spc.loess(x.hs,seq(240,335,.05),span=.005)}

StdVISAxisVenusCut <- function(x.hs){spc.loess(x.hs,seq(375,475,.05),span=.005)}

StdVNIRAxisVenusCut <- function(x.hs){spc.loess(x.hs,seq(534,805,.15),span=.003)}
wl.stdz.cut.funcs <- list(StdUVAxisVenusCut,StdVISAxisVenusCut,StdVNIRAxisVenusCut)

## >  basalt.selector <- sapply(rownames(cset.comps),function(x){x %in% sample.groups[,3]})


MakeModel <- function(x.pipeout,x.comps){
  x.hs <- x.pipeout$spec
  x.pls <- PlsReady(bind('c',x.hs),x.comps)
  x.model <- plsr(comps ~ spectra, data=x.pls, ncomp = 15, validation = "LOO")
}

## multi.pipe.out holds the results of the above function, run on the
## century set. 
## > default.pls <- PlsReady(bind('c',multi.pipe.out$default$spec)[1:100,],cset.comps)
## > onlybase.pls <- PlsReady(bind('c',multi.pipe.out$onlybase$spec)[1:100,],cset.comps)
## > bare.pls <- PlsReady(bind('c',multi.pipe.out$bare$spec)[1:100],cset.comps)
## > onlynorm.pls <- PlsReady(bind('c',multi.pipe.out$onlynorm$spec)[1:100,],cset.comps)
## > defaultflipped.pls <- PlsReady(bind('c',multi.pipe.out$defaultflipped$spec)[1:100,],cset.comps)

## > default.model <- plsr(comps ~ spectra, data=default.pls, ncomp = 15, validation = "LOO")
## > onlybase.model <- plsr(comps ~ spectra, data=onlybase.pls, ncomp = 15, validation = "LOO")
## > bare.model <- plsr(comps ~ spectra, data=bare.pls, ncomp = 15, validation = "LOO")
## > onlynorm.model <- plsr(comps ~ spectra, data=onlynorm.pls, ncomp = 15, validation = "LOO")
## > defaultflipped.model <- plsr(comps ~ spectra, data=defaultflipped.pls, ncomp = 15, validation = "LOO")

Pipeline <- function(x.hs.list){
  s.hs.list <- WvLenStd(x.hs.list,wl.stdz.funcs)
  n.hs.list <- lapply(s.hs.list,IntensityNormalize)
  bline.hs.list <- lapply(n.hs.list, function(x){apply(x,1,BlineDefault)})
  sub.hs.list <- mapply("-",n.hs.list,bline.hs.list)
  f.hs.list <- lapply(sub.hs.list,averageSpots)
  list(spec=f.hs.list,baselines=bline.hs.list)
}

Pipeline.flipped <- function(x.hs.list){
  ## flipped the order of subtracting the baseline and normalizing to
  ## intensity
  s.hs.list <- WvLenStd(x.hs.list,wl.stdz.funcs)
  bline.hs.list <- lapply(s.hs.list, function(x){apply(x,1,BlineDefault)})
  sub.hs.list <- mapply("-",s.hs.list,bline.hs.list)
  n.hs.list <- lapply(sub.hs.list,IntensityNormalize)
  f.hs.list <- lapply(n.hs.list,averageSpots)
  list(spec=f.hs.list,baselines=bline.hs.list)
}

Pipeline.OnlyNormalize <- function(x.hs.list){
  s.hs.list <- WvLenStd(x.hs.list,wl.stdz.funcs)
  n.hs.list <- lapply(s.hs.list,IntensityNormalize)
  ##bline.hs.list <- lapply(n.hs.list, function(x){apply(x,1,BlineDefault)})
  ##sub.hs.list <- mapply("-",n.hs.list,bline.hs.list)
  f.hs.list <- lapply(n.hs.list,averageSpots)
  list(spec=f.hs.list)
}

Pipeline.OnlyBaseline.Cutrange <- function(x.hs.list){
  s.hs.list <- WvLenStd(x.hs.list,wl.stdz.cut.funcs)
  bline.hs.list <- lapply(s.hs.list, function(x){apply(x,1,BlineDefault)})
  sub.hs.list <- mapply("-",s.hs.list,bline.hs.list)
  f.hs.list <- lapply(sub.hs.list,averageSpots)
  list(spec=f.hs.list,baselines=bline.hs.list)
}

Pipeline.OnlyBaseline <- function(x.hs.list){
  s.hs.list <- WvLenStd(x.hs.list,wl.stdz.funcs)
  bline.hs.list <- lapply(s.hs.list, function(x){apply(x,1,BlineDefault)})
  sub.hs.list <- mapply("-",s.hs.list,bline.hs.list)
  f.hs.list <- lapply(sub.hs.list,averageSpots)
  list(spec=f.hs.list,baselines=bline.hs.list)
}

Pipeline.Bare <- function(x.hs.list){
  s.hs.list <- WvLenStd(x.hs.list,wl.stdz.funcs)
  f.hs.list <- lapply(s.hs.list,averageSpots)
  list(spec=f.hs.list)
}

NoiseSpectrum <- function(x.hs, noise.coefficents){
  ## This will noise a whole hyperspec object with the given MTF curve
  x.fft <- apply(x.hs, 1, fft)
  x.noised <- sweep(x.fft, 2, noise.coefficents, "*")
  x.noised.hs <- apply(x.noised, 1, fft, inverse=TRUE)/ncol(x.hs$spc)
  apply(x.noised.hs, 1, as.real)
}

Gen.Pipeline.NoiseUp <- function(noise.coefficents.list){
  
  function(x.hs.list){
    s.hs.list <- WvLenStd(x.hs.list,wl.stdz.funcs)
    f.hs.list <- lapply(s.hs.list,averageSpots)
    noised.hs.list <- mapply(NoiseSpectrum, f.hs.list, noise.coefficents.list)
    list(spec=noised.hs.list)
  }
}

Gen.Pipeline.NoiseUp.Bsub <- function(noise.coefficents.list){
  
  function(x.hs.list){
    s.hs.list <- WvLenStd(x.hs.list,wl.stdz.funcs)
    bline.hs.list <- lapply(s.hs.list, function(x){apply(x,1,BlineDefault)})
    sub.hs.list <- mapply("-",s.hs.list,bline.hs.list)
    f.hs.list <- lapply(sub.hs.list,averageSpots)
    noised.hs.list <- mapply(NoiseSpectrum, f.hs.list, noise.coefficents.list)
    list(spec=noised.hs.list)
  }
}

Gen.Pipeline.TreatmentOne.Bsub <- function(amplitude.reduction.factor){
  
  function(x.hs.list){
    ctr.1 <- function(y){Treatment1(y,amplitude.reduction.factor)}
    x.hs.list <- lapply(x.hs.list, function(x){apply(x,1,ctr.1)})
    s.hs.list <- WvLenStd(x.hs.list,wl.stdz.funcs)
    bline.hs.list <- lapply(s.hs.list, function(x){apply(x,1,BlineDefault)})
    sub.hs.list <- mapply("-",s.hs.list,bline.hs.list)
    f.hs.list <- lapply(sub.hs.list,averageSpots)
    noised.hs.list <- mapply(NoiseSpectrum, f.hs.list, noise.coefficents.list)
    list(spec=noised.hs.list)
  }
}

pipeline.funk.list <- list(onlybase = Pipeline.OnlyBaseline,
                           onlynorm = Pipeline.OnlyNormalize,
                           default = Pipeline,
                           flipped = Pipeline.flipped,
                           bare = Pipeline.Bare)

OneHS <- function(x.data.dir,x.wl.cal.funcs,preprocess.funk){
  raw.hs.list <- loadDataSet(x.data.dir,x.wl.cal.funcs)
  x.pipeout <- preprocess.funk(raw.hs.list)
  bind(direction='c',x.pipeout$spec)
}

Treatments <- function(raw.hs.list, treatment.pipelines){
  treated.list <- lapply(treatment.pipelines, function(x){x(raw.hs.list)})
  lapply(treated.list, function(x){bind(direction='c',x$spec)})
}

## Consider a much more radical preprocessing step: decomposing
## spectra into features based on peak: intensity, location, area.
## THIS DOES NOT WORK. These is some kind of problem in the
## genpipeline function that causes the spectera to come out below
## zero a bunch, and that is totally not okay. The pre-noising works,
## but NOT the post-noising in this most obvious location: TODO!!,
## locate the code that post-noises correctly.
funklist <- list(itt.mtf=Gen.Pipeline.NoiseUp(list(mtf.csv$VIS.UV.ITT.MTF, mtf.csv$VIS.UV.ITT.MTF, mtf.csv$VNIR.ITT.MTF)),
                 mtf.50=Gen.Pipeline.NoiseUp(list(mtf.csv$VIS.UV.MTF.50, mtf.csv$VIS.UV.MTF.50, mtf.csv$VNIR.MTF.50)),
                 mtf.25=Gen.Pipeline.NoiseUp(list(mtf.csv$VIS.UV.MTF.25, mtf.csv$VIS.UV.MTF.25, mtf.csv$VNIR.MTF.25)),
                 mtf.15=Gen.Pipeline.NoiseUp(list(mtf.csv$VIS.UV.MTF.15, mtf.csv$VIS.UV.MTF.15, mtf.csv$VNIR.MTF.15)),
                 mtf.10=Gen.Pipeline.NoiseUp(list(mtf.csv$VIS.UV.MTF.10, mtf.csv$VIS.UV.MTF.10, mtf.csv$VNIR.MTF.10)))

funklist.bsub <- list(itt.mtf=Gen.Pipeline.NoiseUp.Bsub(list(mtf.csv$VIS.UV.ITT.MTF, mtf.csv$VIS.UV.ITT.MTF, mtf.csv$VNIR.ITT.MTF)),
                 mtf.50=Gen.Pipeline.NoiseUp.Bsub(list(mtf.csv$VIS.UV.MTF.50, mtf.csv$VIS.UV.MTF.50, mtf.csv$VNIR.MTF.50)),
                 mtf.25=Gen.Pipeline.NoiseUp.Bsub(list(mtf.csv$VIS.UV.MTF.25, mtf.csv$VIS.UV.MTF.25, mtf.csv$VNIR.MTF.25)),
                 mtf.15=Gen.Pipeline.NoiseUp.Bsub(list(mtf.csv$VIS.UV.MTF.15, mtf.csv$VIS.UV.MTF.15, mtf.csv$VNIR.MTF.15)),
                 mtf.10=Gen.Pipeline.NoiseUp.Bsub(list(mtf.csv$VIS.UV.MTF.10, mtf.csv$VIS.UV.MTF.10, mtf.csv$VNIR.MTF.10)))

PrepSpectrumCSV <- function(filename, snips){
  x.csv <- read.csv(filename,header=FALSE)
  print(snips)
  ## pull out data
  wl.guesses <- as.numeric(x.csv[1,][-seq(1,3)])
  sample.ids <- x.csv[,2][-1]
  spots <- x.csv[,3][-1]
  spectera <- x.csv[-1,-seq(1:3)]

  x.spc <- new("hyperSpec", spc=spectera,
               data=data.frame(list(spot=spots,id=sample.ids)),
               wavelength=wl.guesses)

  uv.start <<- snips$UV[1]
  uv.end <<- snips$UV[2]

  vis.start <<- snips$VIS[1]
  vis.end <<- snips$VIS[2]

  vnir.start <<- snips$VNIR[1]
  vnir.end <<- snips$VNIR[2]

  regions <- list(UV   = x.spc[,,uv.start ~ uv.end],
                  VIS  = x.spc[,,vis.start ~ vis.end],
                  VNIR = x.spc[,,vnir.start ~ vnir.end])
 
 # mapply(transformWLsBy,regions,wl.cal.fcns)
  
}

snips.venus <- list(UV = c(241, 335),
                     VIS = c(375, 475),
                     VNIR = c(535, 806))


snips.group1 <- list(UV = c(223.4,325.97),
                     VIS = c(381.86, 471.03),
                     VNIR = c(494.93,922.47))

snips.group2 <- list(UV = c(223.4, 325.97),
                     VIS = c(381.86, 471.03),
                     VNIR = c(494.93, 927.06))

snips.group3 <- list(UV = c(224.34, 326.92),
                     VIS = c(384.02, 472.53),
                     VNIR = c(494.62, 932.89))

snips.nacl <- list(UV = c(224.53, 325.03),
                   VIS = c(382.54, 472.3),
                   VNIR = c(493.3, 925.96))


group1.wl.cal.fcns <-
  list(uv=function(x){-0.0000024789*x^2 + 0.0546878470*x + 223.4890480838},
       vis=function(x){-0.0000029677*x^2 + 0.0495293357*x + 382.4018320840},
       vnir=function(x){-0.0000086625*x^2 + 0.2286261492*x + 499.1242961682})

group2.wl.cal.funcs <-
  list(uv=function(x){-0.0000024789*x^2 + 0.0546878470*x + 223.4890480838},
       vis=function(x){-0.0000029677*x^2 + 0.0495293357*x + 382.4018320840},
       vnir=function(x){-0.0000085228*x^2 + 0.2290554804*x + 493.4574262496})

group3.wl.cal.funcs <-
  list(uv=function(x){-0.0000024716*x^2 + 0.0551812483*x + 224.2995973677},
       vis=function(x){-0.0000029915*x^2 + 0.0492158813*x + 384.2730695349},
       vnir=function(x){-0.0000095591*x^2 + 0.2335220856*x + 493.8474154048})



Treatment1 <- function(x,amp.red){
  ## x is a vector of spectrum pixels
  y <- vector(mode="numeric", length=length(x))
  for (i in 1:length(x)){
    if (i <= 31){
      local.med <- median(x[1:i+31])
      if (x[i] > local.med){
        y[i] <- amp.red*(x[i]-local.med) + local.med
      }else{
        y[i] <- x[i]
      }
    }else{
      local.med <-  median(x[i-31:i+31])
      if (x[i] > local.med){
        y[i] <- amp.red*(x[i]-local.med) + local.med
      }else{
        y[i] <- x[i]
      }
    }
  }
  y
}

Treatment2 <- function(x,amp.red){
  ## x is a vector of spectrum pixels
  y <- vector(mode="numeric", length=length(x))
  for (i in 1:length(x)){
    if (i <= 31){
      local.med <- median(x[1:i+31])
      if (x[i] > local.med){
        y[i] <- amp.red*(x[i]-local.med) + local.med
      }else{
        y[i] <- sqrt(amp.red)*x[i]
      }
    }else{
      local.med <-  median(x[i-31:i+31])
      if (x[i] > local.med){
        y[i] <- amp.red*(x[i]-local.med) + local.med
      }else{
        y[i] <- sqrt(amp.red)*x[i]
      }
    }
  }
  y
}


SpectrumTransfom <- function(region.list, param.list, line.funk){
  uv.tlist <- MassTransform(list(region.list[[1]]), line.funk, param.list)
  vis.tlist <- MassTransform(list(region.list[[2]]), line.funk, param.list)
  vnir.tlist <- MassTransform(list(region.list[[3]]), line.funk, param.list)

  mapply(list, uv.tlist, vis.tlist, vnir.tlist, SIMPLIFY=FALSE)
}

MassTransform <- function(x.hs,line.funk,param.list){
  mapply(function(x,y){apply(x,1,line.funk,y)}, x.hs, param.list, SIMPLIFY=FALSE)
}
MassTransform.test <- function(x.hs,line.funk,param.list){
  mapply(function(x,y){print(x);print(y)}, x.hs, param.list, SIMPLIFY=FALSE)
}

##  pipelined.treatment1.list <- lapply(cset.treatment1.raw[1:10],Pipeline.OnlyBaseline)
## apply(test.subset.hs,1,Treatment2,.8)
=======

Pipeline <- function(x.hs.list, wl.std.funcs, wl.cal.funcs){
  ## The reason this operates on a list is entirely due to wavelength
  ## standardization; the rest of the stuff could be done on a non
  ## per-region basis. Just call a RegionalHS on the output of
  ## ReadDirectory.
  cal.hs.list <- mapply(transformWLsBy, x.hs.list, wl.cal.funcs)
  ## do.call needs a list of arguments to construct a function call!
  std.hs.list <- mapply(do.call, wl.std.funcs, lapply(cal.hs.list,list))
  #bline.hs.list <- lapply(std.hs.list, function(x){apply(x,1,BaselineSubtract)})
  #sub.hs.list <- mapply("-",std.hs.list,bline.hs.list)
  
  #f.hs.list <- lapply(sub.hs.list,averageSpots)
  unsub.hs.list <- lapply(std.hs.list,averageSpots)
  ## NOTE: currently un-usable, using it to test un-averaged into pls
  list(spec=unsub.hs.list) #baselines=bline.hs.list)
}
>>>>>>> /home/marco/Dropbox/all-code-versions/old-code-collection/fresh-start/preprocess.R
