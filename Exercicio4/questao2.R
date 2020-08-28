rm(list = ls())
library('plot3D')
source('trainperceptron.R')
source('yperceptron.R')

s1 <- 0.4
s2 <- 0.4
nc <- 200

xc1 <- matrix(rnorm(nc * 2), ncol = 2) * s1 + t(matrix((c(2, 2)), ncol = nc, nrow = 2))
xc2 <- matrix(rnorm(nc * 2), ncol = 2) * s2 + t(matrix((c(4, 4)), ncol = nc, nrow = 2))

xc1treinamento <- xc1[sample(nc, nc*0.7, replace=FALSE),]
xc2treinamento <- xc2[sample(nc, nc*0.7, replace=FALSE),]

y1 <- matrix(0, nrow=nc*0.7)
y2 <- matrix(1, nrow=nc*0.7)


xc1teste <- xc1[sample(nc, nc*0.3, replace=FALSE),]
xc2teste <- xc2[sample(nc, nc*0.3, replace=FALSE),]

y1teste <- matrix(0, nrow=nc*0.3)
y2teste <- matrix(1, nrow=nc*0.3)

concatenatedX <- rbind(xc1treinamento, xc2treinamento)
concatenatedY <- rbind(y1, y2)

concatenatedXteste <- rbind(xc1teste, xc2teste)
concatenatedYteste <- rbind(y1teste, y2teste)

plot(xc1treinamento[,1], xc1treinamento[,2], col = 'red', xlim = c(0, 6), ylim = c(0, 6), xlab = 'x_1', ylab = 'x_2')
par(new = T)
plot(xc2treinamento[,1], xc2treinamento[,2], col = 'blue', xlim = c(0, 6), ylim = c(0, 6), xlab = '', ylab = '')

retlist <- trainperceptron(concatenatedX, concatenatedY, 0.02, 0.02, 100, 1)

w <- retlist[[1]]
erro <- retlist[[2]]

seqi<-seq(0,6,0.1)
seqj<-seq(0,6,0.1)
M <- matrix(0,nrow=length(seqi),ncol=length(seqj))

Mteste <- matrix(0,nrow=length(concatenatedXteste),ncol=length(concatenatedXteste))

ci<-0

for (i in seqi) {
  ci<- ci+1
  cj<-0
  for (j in seqj){
    cj<- cj+1
    x<- c(i,j)
    M[ci,cj]<-yperceptron(x,w,1)
  }
}

seqiTeste<-concatenatedXteste[,0]
seqjTeste<-concatenatedXteste[,1]
ci<-0

for (i in seqiTeste) {
  ci<- ci+1
  cj<-0
  for (j in seqjTeste){
    cj<- cj+1
    x<- c(i,j)
    M[ci,cj]<-yperceptron(x,w,1)
  }
}

plot(xc1treinamento[,1],xc1treinamento[,2],col = 'red', xlim = c(0,6), ylim = c(0,6), xlab='x_1', ylab='x_2')
par(new=T)
plot(xc2treinamento[,1], xc2treinamento[,2], col ='blue', xlim = c(0,6), ylim = c(0,6), xlab='', ylab='')
par(new=T)
contour(seqi, seqj, M, xlim = c(0,6), ylim = c(0,6), xlab='', ylab='')

persp3D(seqi, seqj, M, counter=T, theta = 55, phi = 30, r = 40, d = 0.1, expand = 0.5, ltheta = 90, lphi = 180, shade = 0.4, ticktype = 'detailed', nticks=5)

#testes plotados

plot(xc1teste[,1],xc1teste[,2],col = 'red', xlim = c(0,6), ylim = c(0,6), xlab='x_1', ylab='x_2')
par(new=T)
plot(xc2teste[,1], xc2teste[,2], col ='blue', xlim = c(0,6), ylim = c(0,6), xlab='', ylab='')
par(new=T)
contour(seqiTeste, seqjTeste, Mteste, xlim = c(0,6), ylim = c(0,6), xlab='', ylab='')

persp3D(seqiTeste, seqjTeste, M, counter=T, theta = 55, phi = 30, r = 40, d = 0.1, expand = 0.5, ltheta = 90, lphi = 180, shade = 0.4, ticktype = 'detailed', nticks=5)

