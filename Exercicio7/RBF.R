rm(list = ls())
library('corpcor')


########## Função radial Gaussiana
pdfnvar <- function(x, m, K, n) {
  if (n == 1){
    r <- sqrt(as.numeric(K))
    px <- (1/(sqrt(2 * pi * r * r))) * exp(-0.5 * ((x - m)/(r))^2)
  }
  else px <- ((1/(sqrt((2 * pi)^n * (det(K))))) * exp(-0.5 *(t(x - m) %*% (solve(K)) %*% (x - m))))
  return (px)
}
##################################

treinaRBF <- function(xin, yin, p){

   N <- dim(xin) [1] # numero de amostras
   n <- dim(xin) [2] # dimensão da entrada (deve ser maior que 1)
   
   xin <- as.matrix(xin) # garante que xin seja matriz
   yin <- as.matrix(yin) # garante que xin seja matriz
   
   xclust <- kmeans(xin, p)
   
   # Armazena vetores de centros das funções
   m <- as.matrix(xclust$centers)
   covlist <- list()
   
   # Estima matrizes de covariancia para todos os centros
   for (i in 1:p){
     ici <- which(xclust$cluster == i)
     xci <- xin[ici,]
     if (n == 1) covi <- var(xci)
     else covi <- cov(xci)
     covlist[[i]] <- covi
   }
   
   H <- matrix(nrow=N, ncol=p)
   # Calcula matriz H
   for(j in 1:N){
     for (i in 1:p){
       mi <- m[i,]
       covi <- covlist[i]
       H[j,i] <- pdfnvar(xin[j,], mi, covi, n)
     }
   }
   
   Haug <- cbind(1, H)
   W <- pseudoinverse(Haug) %*% yin
   return(list(m, covlist, W, H))
}

YRBF <- function(xin, modRBF){
  
  N <- dim(xin) [1] # numero de amostras
  n <- dim(xin) [2] # dimensão da entrada (deve ser maior que 1)
  m <- as.matrix(modRBF[[1]])
  covlist <- modRBF[[2]]
  p <- length(covlist) # numero de funcoes radiais
  W <- modRBF[[3]]
  
  xin <- as.matrix(xin) # garante que xin seja matriz
  yin <- as.matrix(yin) # garante que xin seja matriz
  
  H <- matrix(nrow=N, ncol=p)
  # Calcula matriz H
  for(j in 1:N){
    for (i in 1:p){
      mi <- m[i,]
      covi <- covlist[i]
      H[j,i] <- pdfnvar(xin[j,], mi, covi, n)
    }
  }
  
  Haug <- cbind(1, H)
  Yhat <- Haug %*% W
  
  return(Yhat)
}


# mykmedias <- function(X, k) {
#   N <- dim(X) [1]
#   n <- dim(X) [2]
#   
#   mi <- sample(N)
#   m <- matrix(nrow=k, ncol=n)
#   m <- X[mi[1:k],]
#   
#   d <- matrix(nrow=k, ncol=1)
#   c <- matrix(nrow=N, ncol=1)
#   for (kk in (1:100)){
#     for (i in (1:N)){
#       xt <- as.matrix(X[i,])
#       for (j in (1:k)){
#         d[j] <- sum((t(xt) - m[j,])^2)
#       }
#       ordd <- order(d, decreasing=FALSE)
#       c[i] <- ordd[1]
#     }
#     for (i in (1:k)){
#       ici <- which(c[] == i)
#       ni <- length(ici)
#       acc <- matrix(0, nrow=1, ncol=n)
#       for (j in (1:ni)){
#         acc <- acc + t(as.matrix(X[ici[j],]))
#       }
#       m[i,] <- acc/ni
#     }
#   }
#   retlist <- list(m,c)
#   return(retlist)
# }