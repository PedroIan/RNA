
rm(list=ls())
library('corpcor')

fnormal1var <- function(x,m,r) ((1/(sqrt(2*pi*r*r)))*exp(-0.5*((x-m)/(2*r))^2))

fnormal2var <- function(x, m, r) ((1/(sqrt((2 * pi)^length(x) * (det(r))))) * exp(-0.5 *(t(x - m) %*% (solve(r)) %*% (x - m))))

rbfSinc <- function() {
  xl <- -10
  xr <- 10
  st <- 1
  p <- 10
  
  faixa <- (xr - xl)/p
  
  c0 <- xl+0.5*faixa
  ci <- matrix()
  
  x <- runif(100,-15,15)
  y <- data.matrix(sin(x)/x) + rnorm(length(x),0,0.05)
  
  ci[1] <- c0
  for (i in 2:p)
    ci[i] <- ci[i - 1]+faixa
  
  H <- matrix(nrow=length(x), ncol=p)
  for (j in 1:length(x))
    for (i in 1:p)
      H[,i] <- fnormal1var(x, ci[i], 0.6)
  
  xrange <- seq(xl, xr, 0.05)
  
  Hrange <- matrix(nrow=length(xrange), ncol=p)
  for (j in 1:length(xrange))
    for (i in 1:p)
      Hrange[,i] <- fnormal1var(xrange, ci[i], 0.6)
  
  ph <- pseudoinverse(H)
  
  w <- ph %*% y
  
  yhat <- H %*% w
  yhatr <- Hrange %*% w
  
  plot(xrange, yhatr)
  par(new=T)
  plot(x, y)
}

trainaRBF <- function(xIn, yIn, centros, kMediasObj) {
  
  p <- nrow(centros)
  
  covlist <- list()
  for (i in 1:p){
    ici <- which(kMediasObj$cluster == i)
    xci <- xIn[ici,]
    if (is.null(nrow(xci))) covi <- matrix(0, ncol = 2, nrow = 2)
    else covi <- cov(xci)
    covlist[[i]] <- covi
  }
  
  m <- as.matrix(centros)
  
  H <- matrix(nrow=nrow(xIn), ncol=p)
  # Calcula matriz H
  for(j in 1:nrow(xIn)){
    for (i in 1:p){
      mi <- m[i,]
      covi <- covlist[[i]]
      if ((class(try(solve(covi),silent=T))=="matrix")) H[j,i] <- fnormal2var(xIn[j,], mi, covi)
      else H[j,i] <- 0
    }
  }
  
  Haug <- cbind(1, H)
  W <- pseudoinverse(Haug) %*% yIn
  
  return(list(m, covlist, W, H))
}


getResult <- function(xIn, modeloRBF){
  
  m <- as.matrix(modeloRBF[[1]])
  covlist <- modeloRBF[[2]]
  p <- length(covlist)
  W <- modeloRBF[[3]]
  
  H <- matrix(nrow=nrow(xIn), ncol=p)
  # Calcula matriz H
  for(j in 1:nrow(xIn)){
    for (i in 1:p){
      mi <- m[i,]
      covi <- covlist[[i]]
      if ((class(try(solve(covi),silent=T))=="matrix")) H[j,i] <- fnormal2var(xIn[j,], mi, covi)
      else H[j,i] <- 0
    }
  }
  
  Haug <- cbind(1, H)
  Yhat <- Haug %*% W
  
  return(Yhat)
}