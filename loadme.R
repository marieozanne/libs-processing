src.dir <- "/home/marco/msl-stats/src/core"

files <- list("import-scope-data.R","interactive.R","preprocess.R","pls.R")

lapply(files,function(x){source(paste(src.dir,x,sep="/"))})

## source("import-scope-data.R")
## source("interactive.R")
## source("preprocess.R")
## source("pls.R")
