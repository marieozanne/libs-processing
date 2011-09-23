## This file contains the RawImport module, responsible for turning
## files on disk into per-spot metadata and hyperspec objects that are
## in some sense "raw:" they reflect on-disk 

ImportRawData <- function(path, reader.fcn){
  reader.fcn(path)
}

ReadDirectory <- function(directory.name, reader.fcn){
  ## returns a list of hyperspec objects
  files <- list.files(directory.name, recursive=TRUE, full.names=TRUE)
  hs.list <- lapply(files, reader.fcn)  
}

## J0Reader/metadata extractor function pair
## --------------------------------------------------

J0FileReader <- function(filename){
  print(paste("Reading",filename))
  file.hs <- read.txt.long(filename,skip=19,nrows=2048,header=FALSE)
  file.hs@data <- cbind(file.hs@data, filename.to.J0Mdat(filename))
  file.hs
}

filename.to.J0Mdat <- function(filename){
  bare.filename <- tail(strsplit(filename,split="/",fixed=TRUE)[[1]],1)
  split.filename <- strsplit(bare.filename,split="[[:space:]]|[.]")[[1]]
  split.filename <- split.filename[split.filename != ""]
  list(id=split.filename[2],
       region=split.filename[3],
       spot=split.filename[4])
}

J0Reader <- function(path){
  ReadDirectory(path, J0FileReader)
}
## --------------------------------------------------

## LIBSDist/metadata extractor function pair
## --------------------------------------------------

filename.to.LIBSDistMdat <- function(filename){
  split.first <- strsplit(filename,split="/",fixed=TRUE)[[1]]
  split.filename <- strsplit(split.first[2],split="_",fixed=TRUE)[[1]]
  split.unique <- strsplit(split.first[3],split=".",fixed=TRUE)[[1]]
  list(id=split.filename[1],
       distance=split.filename[2],
       region=split.filename[3],
       shot=split.unique[1])
}

LIBSDistReader <- function(filename){
  print(paste("Reading",filename))
  file.hs <- read.txt.long(filename,skip=17,nrows=2047,header=FALSE)
  file.hs@data <- cbind(file.hs@data, filename.to.LIBSDistMdat(filename))
  file.hs
}
## --------------------------------------------------


## Utility Functions

LIBSRegionalHS <- function(hs.list){
  ## sorts the output list by region. suitable for LIBS cset-type
  ## datasets.
  list(UV   = Filter(function(w){w$region=="UV"},hs.list),
       VIS  = Filter(function(w){w$region=="VIS"},hs.list),
       VNIR = Filter(function(w){w$region=="VNIR"},hs.list))
}
