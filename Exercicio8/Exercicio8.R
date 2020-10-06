

rm(list = ls())
library('plot3D')
library(caret)
library(ks)
source('treinaRBF.R')
source('treinaELM.R')
source('readData.R')
library('corpcor')
library(RSNNS)


bcw  <- as.matrix(read.table('breast-cancer-wisconsin.data',
                             header = FALSE, sep = ",", skip = 0))

retlist_xy_bcw <- readTable(bcw, (2:10), 11, 4)

x_bcw <- retlist_xy_bcw[[1]]
x_bcw <- escala(x_bcw)
y_bcw <- retlist_xy_bcw[[2]]

trainIndex_bcw <- createDataPartition(y_bcw, p=0.8, list=FALSE)

retlist_ELM_bcw <- treinaELM(x_bcw[trainIndex_bcw,], y_bcw[trainIndex_bcw], 1)

kMediasReturn_bcw <- kmeans(x_bcw[trainIndex_bcw,], 50, 100)
centros_bcw <- kMediasReturn_bcw$centers

kMediasReturn_bcw_random <- kmeans(x_bcw[trainIndex_bcw,], 10, 1)
centros_bcw_random <- kMediasReturn_bcw_random$centers

objetoTreinamentoRBF_bcw <- trainaRBF(x_bcw[trainIndex_bcw,], y_bcw[trainIndex_bcw], centros_bcw, kMediasReturn_bcw)
objetoTreinamentoRBF_bcw_random <- trainaRBF(x_bcw[trainIndex_bcw,], y_bcw[trainIndex_bcw], centros_bcw_random, kMediasReturn_bcw_random)


##############################

heart <- as.matrix(read.table('heart.dat', header = FALSE, sep = " ", skip = 0))

retlist_xy_heart <- readTable(heart, (1:13), 14, 2)

x_heart <- retlist_xy_heart[[1]]
x_heart <- escala(x_heart)
y_heart <- retlist_xy_heart[[2]]

trainIndex_heart <- createDataPartition(y_heart, p=0.8, list=FALSE)

retlist_ELM_heart <- treinaELM(x_heart[trainIndex_heart,], y_heart[trainIndex_heart], 1)

kMediasReturn_heart <- kmeans(x_heart[trainIndex_heart,], 50, 100)
centros_heart <- kMediasReturn_heart$centers

kMediasReturn_heart_random <- kmeans(x_heart[trainIndex_heart,], 10, 1)
centros_heart_random <- kMediasReturn_heart_random$centers

objetoTreinamentoRBF_heart <- trainaRBF(x_heart[trainIndex_heart,], y_heart[trainIndex_heart], centros_heart, kMediasReturn_heart)
objetoTreinamentoRBF_heart_random <- trainaRBF(x_heart[trainIndex_heart,], y_heart[trainIndex_heart], centros_heart_random, kMediasReturn_heart_random)


##############################
##############################
##############################


W_bcw <- retlist_ELM_bcw[[1]]
H_bcw <- retlist_ELM_bcw[[2]]
Z_bcw <- retlist_ELM_bcw[[3]]

yELM_bcw <- YELM(x_bcw[-trainIndex_bcw,], Z_bcw, W_bcw, 1)
yHatTreinamento_bcw <- getResult(x_bcw[-trainIndex_bcw,], objetoTreinamentoRBF_bcw)
yHatTreinamento_bcw_random <- getResult(x_bcw[-trainIndex_bcw,], objetoTreinamentoRBF_bcw_random)

##############################

W <- retlist_ELM_heart[[1]]
H <- retlist_ELM_heart[[2]]
Z <- retlist_ELM_heart[[3]]


yELM_heart <- YELM(x_bcw[-trainIndex_heart,], Z_bcw, W_bcw, 1)
yHatTreinamento_heart <- getResult(x_heart[-trainIndex_heart,], objetoTreinamentoRBF_heart)
yHatTreinamento_heart_random <- getResult(x_heart[-trainIndex_heart,], objetoTreinamentoRBF_heart_random)


##############################
##############################
##############################


for (i in 1:length(yHatTreinamento_bcw)) {
  if (yHatTreinamento_bcw[i] > 0) {
    yHatTreinamento_bcw[i] <- 1
  }
  else {
    yHatTreinamento_bcw[i] <- -1
  }
}

for (i in 1:length(yHatTreinamento_bcw_random)) {
  if (yHatTreinamento_bcw_random[i] > 0) {
    yHatTreinamento_bcw_random[i] <- 1
  }
  else {
    yHatTreinamento_bcw_random[i] <- -1
  }
}

print(paste('Acurácia ELM BCW - ', as.character(100), ' neurônios.', as.character(acuracia(yELM_bcw, y_bcw[-trainIndex_bcw]))))
print(paste('Acurácia RBF BCW - 10 centros', as.character(acuracia(yHatTreinamento_bcw, y_heart[-trainIndex_heart]))))
print(paste('Acurácia RBF BCW - 10 centros - random', as.character(acuracia(yHatTreinamento_bcw_random, y_heart[-trainIndex_heart]))))


##############################

for (i in 1:length(yHatTreinamento_heart)) {
  if (yHatTreinamento_heart[i] > 0) {
    yHatTreinamento_heart[i] <- 1
  }
  else {
    yHatTreinamento_heart[i] <- -1
  }
}

for (i in 1:length(yHatTreinamento_heart_random)) {
  if (yHatTreinamento_heart_random[i] > 0) {
    yHatTreinamento_heart_random[i] <- 1
  }
  else {
    yHatTreinamento_heart_random[i] <- -1
  }
}

print(paste('Acurácia ELM HEART - 100 neurônios.', as.character(acuracia(yELM_heart, y_heart[-trainIndex_heart]))))
print(paste('Acurácia RBF HEART - 10 centros', as.character(acuracia(yHatTreinamento_heart, y_heart[-trainIndex_heart]))))
print(paste('Acurácia RBF HEART - 10 centros - random', as.character(acuracia(yHatTreinamento_heart_random, y_heart[-trainIndex_heart]))))


##############################
d <- diag(2)
match(2, kMediasReturn_bcw$cluster)
which(kMediasReturn_bcw$cluster %in% 1)

p <- sort(unique(kMediasReturn_bcw$cluster))

modeloRBF <- objetoTreinamentoRBF_heart_random
xIn <- x_heart[-trainIndex_heart,]

m <- as.matrix(modeloRBF[[1]])
covlist <- modeloRBF[[2]]
p <- length(covlist)
W <- modeloRBF[[3]]

H <- matrix(nrow=nrow(xIn), ncol=p)
a <- covlist[[2]]
solve(covlist[[2]])
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
