getDistancias <- function(ponto, centroClusters){
  nCluster <- length(centroClusters[,1])
  
  response <- matrix(0, nrow = nCluster)
  
  for(i in 1:nCluster) {
    diffX <- ponto[1] - centroClusters[i, 1]
    diffY <- ponto[2] - centroClusters[i, 2]
    
    response[i] <- sqrt(diffX^2 + diffY^2)
  }
  
  return(response)
}

kMeans <- function(nClusters, xIn, maxIteracao){
  iteracoes <- 1
  
  if(maxIteracao > 0) {
    iteracoes <- maxIteracao
  }
  
  limiteSuperior <- max(max(xIn[,1]), max(xIn[,2]))
  limiteInferior <- min(min(xIn[,1]), min(xIn[,2]))
  
  pontosX <- runif(nClusters, limiteInferior, limiteSuperior)
  pontosY <- runif(nClusters, limiteInferior, limiteSuperior)
  
  clusters <- t(rbind(t(pontosX), t(pontosY)))
  
  clusterOfPoints <- matrix(0, nrow = nrow(xIn))
  
  xIn <- cbind(xIn, clusterOfPoints)
  
  
  for(i in 1:iteracoes){
    
    for(index in 1:nrow(xIn)){
      minIndex <- which.min(getDistancias(xIn[index,], clusters))
      
      clusterOfPoints[index] <- minIndex
    }
    
    xIn[,3] <- clusterOfPoints
    
    for(j in 1:nrow(clusters)){
      a <- xIn[xIn[,3] == j,]
      
      if(is.matrix(a)) {
        xCenter <- sum(a[,1]) / length(a[,1])
        yCenter <- sum(a[,2]) / length(a[,2])
        
        if(is.numeric(xCenter) & is.numeric(yCenter)){
          clusters[j,1] <- xCenter
          clusters[j,2] <- yCenter
          
        } else {
          clusters[j,1] <- 0
          clusters[j,2] <- 0
        }
        
      } else {
        clusters[j,1] <- 0
        clusters[j,2] <- 0
      }
    }
  }
  
  retList <- list(clusters, xIn)
  
  return(retList)
  
}
