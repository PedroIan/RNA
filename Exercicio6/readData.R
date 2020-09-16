readTable <- function(matrixAll, inCols, outCol, outDict) {
  matrixCopy <- na.exclude(matrixAll)
  x <- as.matrix(matrixCopy[,inCols])
  y <- matrixCopy[,outCol]
  
  for (i in nrow(matrixCopy):1){
    if ('?' %in% matrixCopy[i,]) matrixCopy <- matrixCopy[-i,]
  }
  
  for (i in 1:nrow(matrixCopy)) {
    if (y[i] == outDict) {
      y[i] <- 1
    }
    else {
      y[i] <- -1
    }
  }
  
  y <- as.numeric(y)
  
  apply(x, 2, as.numeric)
  sapply(x, as.numeric)
  class(x) <- "numeric"
  
  return(list(x,y))
}