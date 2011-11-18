## These hardcoded functions take an index on their denoted wavelength
## region (index ranges from 1 to # of channels) and turn that into a
## hand-callibrated estimate of what wavelength actually lives on that
## channel. They are specific to each dataset.

## Century Set

cset.wl.cal.funcs <-
  list(uv=function(x){ -(.0000025225*x^2) + (.0548171309*x) + 223.5139936039},#uv
       vis=function(x){ -(.0000029704*x^2) + (.0495324943*x) + 382.0626425975},#vis
       vnir=function(x){ -(.0000085888*x^2) + (.2292115964*x) + 493.6728296224})#vnir

cset.data.dir <- "/home/marco/data-cleaned/century-set"
cset.comps.file <- "/home/marco/msl-stats/stashed-objects/century-comps.rtable"
cset.major.comps.file <- "/home/marco/msl-stats/stashed-objects/century-major-comps.rtable"

## Phylosilicates

phy.wl.cal.funcs <-
  list(uv=function(x){-(0.0000025195*x^2) + (0.0547961374*x) + 223.5525816833},
       vis=function(x){-(0.0000030176*x^2) + (0.0496391630*x) + 382.0359185391},
       vnir=function(x){-(0.0000086874*x^2) + (0.2293814797*x) + 493.7574623181})

phy.data.dir <- "/home/marco/clean-data/phylo"
#phy.comps.file <- "/home/marco/msl-stats/stashed-objects/phy-comps.rtable"
phy.major.comps.file <- "/home/marco/msl-stats/stashed-objects/phy-major-comps.rtable"

## Sedimentary Rocks

sed.wl.cal.funcs <-
  list(uv=function(x){-(2.5095E-06*x^2) + (0.054779329*x) + 223.5111416},
       vis=function(x){-(0.00000298*x^2) + (0.049557027*x) + 382.092352},
       vnir=function(x){-(8.7904E-06*x^2) + (0.229538635*x) + 493.5942132})

sed.data.dir <- "/home/marco/clean-data/sed-spectra"
#sed.comps.file <- "/home/marco/msl-stats/data/sed-sample-comps.csv"
sed.major.comps.file <- "/home/marco/msl-stats/stashed-objects/sed-major-comps.rtable"

## Distance Corrected Data

dcorr.wl.cal.funcs <-
  list(uv = function(x){ -0.0000025638*x^2 + 0.0549175133*x + 223.4352350503},
       vis = function(x){ -0.0000029836*x^2 + 0.0495470362*x + 382.0964302843},
       vnir = function(x) {-0.0000086585*x^2 + 0.2294193674*x + 493.6847951134})

dcorr.4m.data.dir <- "/home/marco/clean-data/distance-data/4m"
dcorr.6.5m.data.dir <- "/home/marco/clean-data/distance-data/6.5m"
dcorr.9m.data.dir <- "/home/marco/clean-data/distance-data/9m"

dd.major.comps.file <- "/home/marco/msl-stats/stashed-objects/distance-data-comps.rtable"

## PET Class -- Distance Version

## Already represented here: for the distance version of the PET class, use dcorr.wl.cal.funcs.

pet.distance.data.dir <- "/home/marco/clean-data/pet-class-distance/"

## PET Class

pet.wl.cal.funcs <-
  list(uv = function(x){ -0.0000022520*x^2 + 0.0538525939*x + 224.3191519065},
       vis = function(x){-0.0000030299*x^2 + 0.0496576016*x + 382.0182150698},
       vnir = function(x) {-0.0000087351*x^2 + 0.2294397854*x + 493.4164076778})

pet.data.dir <- "/home/marco/clean-data/pet-class/"
