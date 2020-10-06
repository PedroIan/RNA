
rm(list=ls())
library('corpcor')
library(mlbench)

fnormal1var <- function(x,m,r) ((1/(sqrt(2*pi*r*r)))*exp(-0.5*((x-m)/(2*r))^2))

fnormal2var <- function(x, m, r) ((1/(sqrt((2 * pi)^length(x) * (det(r))))) * exp(-0.5 *(t(x - m) %*% (solve(r)) %*% (x - m))))

rbfSinc <- function(centros) {
  xl <- -10
  xr <- 10
  p <- centros
  
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
  
  erro <- 0
  for (i in 1:length(yhat)) {
    erro <- erro+(y[i] - yhat[i])^2
  }
  erro <- erro/length(yhat)
  print(erro)
  
  plot(xrange, yhatr,xlim = c(-10, 10), ylim = c(-0.5, 1.5))
  par(new=T)
  plot(x, y, xlim = c(-10, 10), ylim = c(-0.5, 1.5))
}

trainaRBF <- function(xIn, yIn, centros, kMediasObj) {
  
  p <- unique(kMediasObj$cluster)
  
  covlist <- list()
  for (i in 1:length(p)){
    ici <- sort(which(kMediasObj$cluster %in% i))
    xci <- xIn[ici,]
    if (is.null(nrow(xci))) {
      covi <- matrix(1, ncol = 2, nrow = 2)
    }
    else {
      covi <- cov(xci)
    }
    covlist[[i]] <- covi
  }
  
  m <- as.matrix(kMediasObj$centers)
  
  H <- matrix(nrow=nrow(xIn), ncol=length(p))
  # Calcula matriz H
  for(j in 1:nrow(xIn)){
    for (i in 1:length(p)){
      mi <- m[i,]
      covi <- covlist[[i]]
      if (class(try(solve(covi),silent=TRUE)) == "matrix") {
        H[j,i] <- fnormal2var(xIn[j,], mi, covi)
      }
      else {
        H[j,i] <- 0.0
      }
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
      if (class(try(solve(covi),silent=T))=="matrix") H[j,i] <- fnormal2var(xIn[j,], mi, covi)
      else H[j,i] <- 0
    }
  }
  
  Haug <- cbind(1, H)
  Yhat <- Haug %*% W
  
  return(Yhat)
}

treinarTodasAsClasses <- function(kCentros) {
  cores <- rainbow(6)
  
  dnormals <- mlbench.2dnormals(200)
  xor <- mlbench.xor(100)
  circle <- mlbench.circle(100)
  spirals <- mlbench.spirals(100,sd = 0.05)
  
  allEntrada <- list(dnormals, xor, circle, spirals)
  
  for(class in allEntrada) {
    xIn <- class$x
    yIn <- as.numeric(class$classes)
    
    for (i in 1:length(yIn)) {
      if (yIn[i] == yIn[1]) {
        yIn[i] <- 1
      }
      else {
        yIn[i] <- -1
      }
    }
    
    kMediasReturn <- kmeans(xIn, kCentros, 100)
    centros <- kMediasReturn$centers
    
    objetoTreinamentoRBF <- trainaRBF(xIn, yIn, centros, kMediasReturn)
    
    yHatTreinamento <- getResult(xIn, objetoTreinamentoRBF)
    
    
    seqx1x2 <- seq(-4, 4, 0.1)
    lseq <- length(seqx1x2)
    MZ <- matrix(0, nrow = lseq, ncol = lseq)
    
    for (i in 1:lseq) {
      for(j in 1:lseq) {
        x1 <- seqx1x2[i]
        x2 <- seqx1x2[j]
        x1x2 <- matrix((cbind(x1, x2)), nrow = 1)
        MZ[i, j] <- getResult(x1x2, objetoTreinamentoRBF)
      }
    }
    
    erro <- 0
    for (i in 1:length(yHatTreinamento)) {
      erro <- erro+(yIn[i] - yHatTreinamento[i])^2
    }
    erro <- erro/length(yHatTreinamento)
    print(erro)
    
    plot(xIn[,1],xIn[,2], col = cores[yIn+2], xlim = c(-4,4), ylim = c(-4,4), xlab='x', ylab='y')
    par(new = TRUE)
    contour(seqx1x2, seqx1x2, MZ, nlevels = 1, xlim = c(-4, 4), ylim = c(-4, 4))
    
    Sys.sleep(3)
    persp3D(seqx1x2,seqx1x2,MZ,counter=T,theta = 55, phi = 30, r = 40,d = 0.1, expand = 0.5, ltheta = 90, lphi = 180, shade = 0.4,ticktype = 'detailed', nticks=5)
    
  }
  
}

trainaXvezes <- function(xIn, yIn, porcentage = 0.7, ocorrencias = 1) {
  
  retList <- list()
  for (i in 1:ocorrencias) {
    
    trainIndex <- createDataPartition(yIn, p=porcentage, list=FALSE)
    
    kMediasReturn <- kmeans(xIn[trainIndex,], 20, 100)
    centros <- kMediasReturn$centers
    centros[centros %in% 0] <- 1
    
    kMediasReturn$centers <- centros
    
    kMediasReturn_random <- kmeans(xIn[trainIndex,], 50, 1)
    centros_random <- kMediasReturn_random$centers
    centros_random[centros_random %in% 0] <- 1
    
    kMediasReturn_random$centers <- centros_random
    
    retList <- c(retList, trainaRBF(xIn[trainIndex,], yIn[trainIndex], centros, kMediasReturn))
    retList <- c(retList, trainaRBF(xIn[trainIndex,], yIn[trainIndex], centros_random, kMediasReturn_random))
    
  }
  
  return (retList)
}