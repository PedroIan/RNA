readTable <- function(matrixAll, inCols, outCol, outDict) {
  matrixCopy <- matrixAll
  
  for (i in nrow(matrixCopy):1){
    if (('?' %in% matrixCopy[i,]) | NA %in% matrixCopy[i,]) matrixCopy <- matrixCopy[-i,]
  }
  
  x <- matrix(as.numeric(matrixCopy[,inCols]), nrow = nrow(matrixCopy), ncol = length(inCols))
  y <- matrixCopy[,outCol]
  
  
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