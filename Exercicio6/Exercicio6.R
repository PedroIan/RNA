rm(list = ls())
library('plot3D')
library(caret)
library(ks)
source('treinaELM.R')
source('trainperceptron.R')
source('yperceptron.R')
source('readData.R')


nNeuronios <- 10

wdbc <- as.matrix(read.table('wdbc.data', header = FALSE, sep = ",", skip = 0))

retlist <- readTable(wdbc, (3:32), 2, 'M')

x_wdbc <- retlist[[1]]
y_wdbc <- retlist[[2]]

trainIndex <- createDataPartition(y_wdbc, p=0.8, list=FALSE)

retlist <- treinaELM(x_wdbc[trainIndex,], y_wdbc[trainIndex], nNeuronios, 1)

W_wdbc <- retlist[[1]]
H_wdbc <- retlist[[2]]
Z_wdbc <- retlist[[3]]

yELM_wdbc <- YELM(x_wdbc[-trainIndex,], Z_wdbc, W_wdbc, 1)

print(paste('Acurácia WDBC - ', as.character(nNeuronios), ' neurônios.'))
print(acuracia(yELM_wdbc, y_wdbc[-trainIndex]))

##############################

wpbc <- as.matrix(read.table('wpbc.data', header = FALSE, sep = ",", skip = 0))

retlist <- readTable(wpbc, (4:34), 2, 'R')

x_wpbc <- retlist[[1]]
y_wpbc <- retlist[[2]]

trainIndex <- createDataPartition(y_wpbc, p=0.8, list=FALSE)

retlist <- treinaELM(x_wpbc[trainIndex,], y_wpbc[trainIndex], nNeuronios, 1)

W_wpbc <- retlist[[1]]
H_wpbc <- retlist[[2]]
Z_wpbc <- retlist[[3]]

yELM_wpbc <- YELM(x_wpbc[-trainIndex,], Z_wpbc, W_wpbc, 1)

print(paste('Acurácia WPBC - ', as.character(nNeuronios), ' neurônios.'))
print(acuracia(yELM_wpbc, y_wpbc[-trainIndex]))

##############################

bcw  <- as.matrix(read.table('breast-cancer-wisconsin.data',
                             header = FALSE, sep = ",", skip = 0))

retlist <- readTable(bcw, (2:10), 11, 4)

x_bcw <- retlist[[1]]
y_bcw <- retlist[[2]]

trainIndex <- createDataPartition(y_bcw, p=0.8, list=FALSE)

retlist <- treinaELM(x_bcw[trainIndex,], y_bcw[trainIndex], nNeuronios, 1)

W_bcw <- retlist[[1]]
H_bcw <- retlist[[2]]
Z_bcw <- retlist[[3]]

yELM_bcw <- YELM(x_bcw[-trainIndex,], Z_bcw, W_bcw, 1)

print(paste('Acurácia WPBC - ', as.character(nNeuronios), ' neurônios.'))
print(acuracia(yELM_bcw, y_bcw[-trainIndex]))

##############################

heart <- as.matrix(read.table('heart.dat', header = FALSE, sep = " ", skip = 0))

retlist <- readTable(bcw, (1:13), 14, 4)

x_heart <- retlist[[1]]
y_heart <- retlist[[2]]

trainIndex <- createDataPartition(y_bcw, p=0.8, list=FALSE)

retlist <- treinaELM(x_heart[trainIndex,], y_heart[trainIndex], nNeuronios, 1)

W <- retlist[[1]]
H <- retlist[[2]]
Z <- retlist[[3]]

yELM_heart <- YELM(x_bcw[-trainIndex,], Z_bcw, W_bcw, 1)
