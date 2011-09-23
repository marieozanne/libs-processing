## --------------------------------------------------
## Slice up the data proper with plyr:
## > str(ddply(sloppy.frame[1:100,2040:2052],.(name,spectrum),mean))

## I've figured out the reshape! got it working on a single
## sample. this is how to do it, using the "reshape" package (melt and
## cast)

## First, for testing purposes, we subset out only ONE sample from the
## whole "sloppy frame." this gives us one row per spot, per spectrum
## taken.

## kitty <- subset(sloppy.frame,name=="DIG002")

## Next, melt "kitty," (our single sample) just using the defaults:

## grr <- melt(kitty)

## Now, finally, glue the columns together like we want! name and spot
## are the id's...

## testcase_onesample <- cast(grr, name + spot ~ ...)

## this has five rows, one for each spot, and 6146 columns, one for
## each spectrum/pixel and columns for the name and the spot. this is
## exactly what I want, for each sample in the data set.
## --------------------------------------------------

## futher sloppiness: this will give us 99 shots in a data frame, in the UV spectrum, without the 
## first sample because that screws it up by being repeated.
## oneset <- subset(sloppy.frame,(X2.1!="Fe" & X3.1=="UV" & X4.1=="0000000" & X2.1 != "DIG001"))
## filter columns by a logical vector?

## df <-  df[, sapply(df, is.numeric)]
## BE AWARE:
## to get a numeric matrix from as.matrix, must feed in only numeric
## data frame columns: The method for data frames will return a
## character matrix if there is any non-(numeric/logical/complex)
## column, applying format to non-character columns.  (from ?as.matrix)


seperateEvensAndOdds <- function(cset.all.hs,comps.file){
  ## these are logical vectors with TRUE in the odds place, and then
  ## the evens place, respectively.
  select.odds <- rep(c(TRUE,FALSE), 50)
  select.evens <- rep(c(FALSE,TRUE), 50)

  ## we need to limit the cset to the first 100 lines of samples,
  ## because we only have compositions for the samples and there's an
  ## extra line with the iron reference in there.
  cset.samples.hs <- cset.all.hs[1:100]
    
  ## Now, we convert the hs to a matrix, and then use our logical
  ## vectors from above to subset out the appropriate rows.
  cset.odds.matrix <- cset.samples.hs[[]][select.odds,]
  cset.evens.matrix <- cset.samples.hs[[]][select.evens,]


  cset.comps.majors <- csetMajorComps(comps.file)
  ## As above, with the compositions:
  comp.odds.matrix <- as.matrix(cset.comps.majors)[select.odds,]
  comp.evens.matrix <- as.matrix(cset.comps.majors)[select.evens,]
  
  ## Set the rownames in each matrix to their sample number, to make
  ## our lives easier.
  rownames(cset.odds.matrix) <- seq(1,99,by=2)
  rownames(cset.evens.matrix) <- seq(2,100,by=2)
  
  list(odds.matrix=cset.odds.matrix,evens.matrix=cset.evens.matrix,comp.odds=comp.odds.matrix,comp.evens=comp.evens.matrix)
}

