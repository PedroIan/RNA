rm(list = ls())
library('plot3D')
source('trainperceptron.R')
source('yperceptron.R')

data("iris")

y1 <- matrix(0, nrow=50)
y2 <- matrix(1, nrow=100)

yAll <- rbind(y1,y2)

allButSpecies <- rbind(iris[,1],iris[,2],iris[,3],iris[,4], yAll[,1])
allButSpecies <- t(allButSpecies)

nc <- length(allButSpecies[,1])

allTeste <- allButSpecies[sample(nc, nc*0.3, replace=FALSE),]
allTreinamento <- allButSpecies[sample(nc, nc*0.7, replace=FALSE),]

allErros <- matrix(0, nrow=100)

for (j in 1:101){
  
  allTeste <- allButSpecies[sample(nc, nc*0.3, replace=FALSE),]
  allTreinamento <- allButSpecies[sample(nc, nc*0.7, replace=FALSE),]
  
  retlist <- trainperceptron(allTreinamento[,1:4], allTreinamento[,5], 0.02, 0.01, 100, 1)
  w <- retlist[[1]]
  yhat <- matrix(0, nrow=length(allTeste[,1]))
  
  countErrados <- 0
  
  tamanhoTestes <- length(allTeste[,1])
  
  for (i in 1:tamanhoTestes) {
    x<- allTeste[i,1:4]
    yhat[i]<-yperceptron(x,w,1)
    
    if(yhat[i] != allTeste[i,5]) {
      countErrados <- countErrados + 1
    }
    
  }
  
  erroPercentual <- countErrados / tamanhoTestes
  allErros[j] <- erroPercentual * 100
}

plot(1:101,allErros,col = 'red', xlim = c(0,101), ylim = c(0,3), xlab='x_1', ylab='x_2')
var(allErros)