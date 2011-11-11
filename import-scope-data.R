ReadDirectory <- function(directory.name, reader.fcn){
  ## returns a list of hyperspec objects
  files <- list.files(directory.name, recursive=TRUE, full.names=TRUE)
  hs.list <- lapply(files, reader.fcn)  
}

## J0Reader/metadata extractor function pair
## --------------------------------------------------

J0Reader <- function(filename){
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
## --------------------------------------------------

## LIBSDist/metadata extractor function pair
## --------------------------------------------------

filename.to.LIBSDistMdat <- function(filename){
  split.first <- rev(strsplit(filename,split="/",fixed=TRUE)[[1]])
  split.filename <- strsplit(split.first[2],split="_",fixed=TRUE)[[1]]
  split.unique <- strsplit(split.first[1],split=".",fixed=TRUE)[[1]]
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
