HSconvert <- function(i, wl.df, int.df){
  new("hyperSpec", spc=as.matrix(int.df[i,3:ncol(int.df)]), wavelength=as.numeric(wl.df[i,3:ncol(wl.df)]))
}
## To move in intensities...

HSInterpolate <- function(x.hs){
  spc.loess(x.hs, c(seq(7020, 7105, 5),
                    seq(7106, 7118, .1),
                    seq(7118.5, 7140, .5),
                    seq(7141, 7220, 4)))}

HSInterpolateSmall <- function(x.hs){
  spc.loess(x.hs, c(seq(7106, 7118, .1)))
}
                  

## Step -1: read in data from csv's
## g1.wl.csv <- read.csv("/home/marco/projects/xanes-pls/ev1.csv",header=FALSE)
## g1.int.csv <- read.csv("/home/marco/projects/xanes-pls/int1.csv",header=FALSE)

## Step 0: Subset out the orientations we don't care about
## g1.y.raw.wl <- subset(g1.wl.csv, V2 == "Y")
## g1.y.raw.int <- subset(g1.int.csv, V2 == "Y")

## Step 1: Convert csv files into list of .hs objects
## g1.hs.list <- lapply(1:nrow(g1.y.raw.wl),HSconvert,g1.y.raw.wl,g1.y.raw.int)

## Step 2: Interpolate list of .hs objects onto a standard wavelength
## axis:
## g1.std.hs.list <- lapply(g1.hs.list,HSInterpolate)

## Step 3: Bind together everyone into one happy hyperspec object
## group1.hs <- bind(direction='r',g1.std.hs.list)

MakeXanesHs <- function(wvlens.file, intensities.file, orientation, wave.fun){
  x.wl.csv <- read.csv(wvlens.file,header=FALSE)
  x.int.csv <- read.csv(intensities.file,header=FALSE)

  x.o.raw.wl <- subset(x.wl.csv, V2 == orientation)
  x.o.raw.int <- subset(x.int.csv, V2 == orientation)

  x.hs.list <- lapply(1:nrow(x.o.raw.wl), HSconvert, x.o.raw.wl, x.o.raw.int)

  x.std.hs.list <- lapply(x.hs.list, wave.fun)

  x.hs <- bind(direction='r',x.std.hs.list)

  x.hs$ID <- as.character.factor(x.o.raw.wl$V1)
  x.hs
}

NamedPreds <- function(x.pls, x.hs, ncomp){
  preds.vector <- predict(x.pls)[,,ncomp]
  data.frame(predval=preds.vector,ID=x.hs$ID)
}

AllCompPreds <- function(x.pls, x.hs){
  preds.df <- data.frame(predict(x.pls, x.hs[[]]))
  rownames(preds.df) <- x.hs$ID
  preds.df
}

AllCompPredsMat <- function(x.pls, y.mat, y.hs){
  preds.df <- data.frame(predict(x.pls, y.mat))
  rownames(preds.df) <- y.hs$ID
  preds.df
}

## > group1.hs <- MakeXanesHs("/home/marco/projects/xanes-pls/ev1.csv", "/home/marco/projects/xanes-pls/int1.csv", "Y")


## >  group2.hs <- MakeXanesHs("/home/marco/projects/xanes-pls/ev2.csv", "/home/marco/projects/xanes-pls/int2.csv", "Y")
