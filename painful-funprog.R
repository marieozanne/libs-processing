## Functional Programming in R is Broken
## ------------------------------------------------
## compose <- function(f,g){
##   force(f);force(g)
##   function(x){f(g(x))}
## }

## Curry <- function (f, ..., .left = TRUE)
## {
##    .orig = list(...)
##    function(...) {
##        if (.left) {
##            args <- c(.orig, list(...))
##        }
##        else {
##            args <- c(list(...), .orig)
##        }
##        do.call(f, args)
##    }
## }

Papply <- function (f, .orig, .left = TRUE){
  function(...) {
    if (.left) {
      args <- c(.orig, list(...))
    }
    else {
      args <- c(list(...), .orig)
    }
    do.call(f, args)
  }
}

MakePipelineList <- function(region.list){
  lapply(region.list,GenPipelineFunk)
}
## Iterative function application:
Funcall <- function(f, ...) f(...)
## Compute log(exp(acos(cos(0))

GenPipelineFunk <- function(region,x.hs){
  Wvfun <- Papply(WvlenStandardize,pipeargs$WvlenStandardize$region)
  BLfun <- Papply(BaseLineSubtractor,pipeargs$BaseLineSubtractor$region)
  ## Reduce(Funcall,list(
  ##                     averageSpots,
  ##                     normalizeRows,
  ##                     BLfun,
  ##                     Wvfun
  ##                    ),x.hs,right=TRUE)
  Wvfun(x.hs)
}

Pipeline.UV <- PipelineGen(Papply(BaseLineSubtractor,UV.BL),StdUVAxis)

pipeargs <- list(BaseLineSubtractor=list(uv=UV.BL,vnir=VNIR.BL,vis=VIS.BL),
                 WvlenStandardize=list(uv=StdUVAxis,vnir=StdVNIRAxis,vis=StdVISAxis))

WvlenStandardize <- function(std.func,x.hs){
  print("Standardizing Wavelength")
  std.func(x.hs)
}

PipelineGen <- function(BLSub,Standardizer){
  function(x.hs){
    std.hs <- Standardizer(x.hs)
    blsub.hs <- BLSub(std.hs)
    averageSpots(normalizeRows(blsub.hs))
  }
}
