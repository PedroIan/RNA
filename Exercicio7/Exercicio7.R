
rm(list = ls())
source('treinaRBF.R')
library('corpcor')
library('plot3D')
library(mlbench)
library(caret)
library(ks)

cores <- rainbow(6)

dnormals <- mlbench.2dnormals(200)
xor <- mlbench.xor(100)
circle <- mlbench.circle(100)
spirals <- mlbench.spirals(100,sd = 0.05)

allEntrada <- list(dnormals, xor, circle, spirals)

#rbfSinc()

xIn <- circle$x
yIn <- as.numeric(circle$classes)

for (i in 1:length(yIn)) {
  if (yIn[i] == yIn[1]) {
    yIn[i] <- 1
  }
  else {
    yIn[i] <- -1
  }
}

kMediasReturn <- kmeans(xIn, 20, 100)
centros <- kMediasReturn$centers

teste2 <- trainaRBF(xIn, yIn, centros, kMediasReturn)

teste3 <- getResult(xIn, teste2)


seqx1x2 <- seq(-4, 4, 0.1)
lseq <- length(seqx1x2)
MZ <- matrix(0, nrow = lseq, ncol = lseq)

for (i in 1:lseq) {
  for(j in 1:lseq) {
    x1 <- seqx1x2[i]
    x2 <- seqx1x2[j]
    x1x2 <- matrix((cbind(x1, x2)), nrow = 1)
    MZ[i, j] <- getResult(x1x2, teste2)
  }
}

plot(xIn[,1],xIn[,2], col = cores[yIn+2], xlim = c(-4,4), ylim = c(-4,4), xlab='x', ylab='y')
par(new = TRUE)
contour(seqx1x2, seqx1x2, MZ, nlevels = 1, xlim = c(-4, 4), ylim = c(-4, 4))

Sys.sleep(3)
persp3D(seqx1x2,seqx1x2,MZ,counter=T,theta = 55, phi = 30, r = 40,d = 0.1, expand = 0.5, ltheta = 90, lphi = 180, shade = 0.4,ticktype = 'detailed', nticks=5)
