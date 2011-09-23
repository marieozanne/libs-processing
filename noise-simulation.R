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

