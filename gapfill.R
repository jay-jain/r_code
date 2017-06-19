# See repository: https://git.math.uzh.ch/florian.gerber/gapfill/tree/master 

library(gapfill)

ptm <- proc.time()
## Not run: 
out <- Gapfill(ndvi, clipRange = c(0, 1))

## look at input and output
str(ndvi)
str(out)
Image(ndvi)
Image(out$fill)

## run on 2 cores in parallel
if(require(doParallel)){
  registerDoParallel(2)
  out <- Gapfill(ndvi, dopar = TRUE)
}

## return also the prediction interval
out <- Gapfill(ndvi, nPredict = 3, predictionInterval = TRUE)

## dimension has changed according to 'nPredict = 3'
dim(out$fill)

## clip values outside the valid parameter space [0,1].
out$fill[out$fill < 0] <- 0
out$fill[out$fill > 1] <- 1

## images of the output:
## predicted NDVI
Image(out$fill[,,,,1])
## lower bound of the prediction interval
Image(out$fill[,,,,2])
## upper bound of the prediction interval
Image(out$fill[,,,,3])
## prediction interval length
Image(out$fill[,,,,3] - out$fill[,,,,2])

proc.time() - ptm

## End(Not run)
