

rm(list = ls())
library('plot3D')
library(caret)
library(ks)
source('treinaRBF.R')
source('treinaELM.R')
source('readData.R')
library('corpcor')
library(RSNNS)

##############################
# Leitura dos dados

bcw  <- as.matrix(read.table('breast-cancer-wisconsin.data',
                             header = FALSE, sep = ",", skip = 0))

retlist_xy_bcw <- readTable(bcw, (2:10), 11, 4)

x_bcw <- retlist_xy_bcw[[1]]
x_bcw <- escala(x_bcw)
y_bcw <- retlist_xy_bcw[[2]]

##############################

heart <- as.matrix(read.table('heart.dat', header = FALSE, sep = " ", skip = 0))

retlist_xy_heart <- readTable(heart, (1:13), 14, 2)

x_heart <- retlist_xy_heart[[1]]
x_heart <- escala(x_heart)
y_heart <- retlist_xy_heart[[2]]

##############################

acuracia_elm_bcw <- c()
acuracia_bcw <- c()
acuracia_bcw_random <- c()

acuracia_elm_heart <- c()
acuracia_heart <- c()
acuracia_heart_random <- c()

for (i in 1:10) {
  ##############################
  ##############################
  ##############################
  # Treinamento
  
  
  trainIndex_bcw <- createDataPartition(y_bcw, p=0.65, list=FALSE)
  
  retlist_ELM_bcw <- treinaELM(x_bcw[trainIndex_bcw,], y_bcw[trainIndex_bcw], 1)
  
  kMediasReturn_bcw <- kmeans(x_bcw[trainIndex_bcw,], 20, 100)
  centros_bcw <- kMediasReturn_bcw$centers
  
  kMediasReturn_bcw_random <- kmeans(x_bcw[trainIndex_bcw,], 60, 1)
  centros_bcw_random <- kMediasReturn_bcw_random$centers
  
  objetoTreinamentoRBF_bcw <- trainaRBF(x_bcw[trainIndex_bcw,], y_bcw[trainIndex_bcw], centros_bcw, kMediasReturn_bcw)
  objetoTreinamentoRBF_bcw_random <- trainaRBF(x_bcw[trainIndex_bcw,], y_bcw[trainIndex_bcw], centros_bcw_random, kMediasReturn_bcw_random)
  
  ##############################
  
  trainIndex_heart <- createDataPartition(y_heart, p=0.65, list=FALSE)
  
  retlist_ELM_heart <- treinaELM(x_heart[trainIndex_heart,], y_heart[trainIndex_heart], 1)
  
  kMediasReturn_heart <- kmeans(x_heart[trainIndex_heart,], 20, 100)
  centros_heart <- kMediasReturn_heart$centers
  centros_heart[centros_heart %in% 0] <- 1
  kMediasReturn_heart$centers <- centros_heart
  
  kMediasReturn_heart_random <- kmeans(x_heart[trainIndex_heart,], 60, 1)
  centros_heart_random <- kMediasReturn_heart_random$centers
  centros_heart_random[centros_heart_random %in% 0] <- 1
  kMediasReturn_heart_random$centers <- centros_heart_random
  
  objetoTreinamentoRBF_heart <- trainaRBF(x_heart[trainIndex_heart,], y_heart[trainIndex_heart], centros_heart, kMediasReturn_heart)
  objetoTreinamentoRBF_heart_random <- trainaRBF(x_heart[trainIndex_heart,], y_heart[trainIndex_heart], centros_heart_random, kMediasReturn_heart_random)
  
  
  ##############################
  ##############################
  ##############################
  # Resultados
  
  yELM_bcw <- YELM(x_bcw[-trainIndex_bcw,], retlist_ELM_bcw, 1)
  yHatTreinamento_bcw <- getResult(x_bcw[-trainIndex_bcw,], objetoTreinamentoRBF_bcw)
  yHatTreinamento_bcw_random <- getResult(x_bcw[-trainIndex_bcw,], objetoTreinamentoRBF_bcw_random)
  
  ##############################
  
  yELM_heart <- YELM(x_heart[-trainIndex_heart,], retlist_ELM_heart, 1)
  yHatTreinamento_heart <- getResult(x_heart[-trainIndex_heart,], objetoTreinamentoRBF_heart)
  yHatTreinamento_heart_random <- getResult(x_heart[-trainIndex_heart,], objetoTreinamentoRBF_heart_random)
  
  
  ##############################
  ##############################
  ##############################
  print(paste('Acurácia pontual.', as.character(i)))
  print('...')
  #print(paste('ELM BCW - 300 neurônios.', as.character(acuracia(yELM_bcw, y_bcw[-trainIndex_bcw]))))
  #print(paste('RBF BCW - 20 centros', as.character(acuracia(yHatTreinamento_bcw, y_heart[-trainIndex_heart]))))
  #print(paste('RBF BCW Random - 30 centros - random', as.character(acuracia(yHatTreinamento_bcw_random, y_heart[-trainIndex_heart]))))
  
  ##############################
  
  #print(paste('ELM HEART - 300 neurônios.', as.character(acuracia(yELM_heart, y_heart[-trainIndex_heart]))))
  #print(paste('RBF HEART - 20 centros', as.character(acuracia(yHatTreinamento_heart, y_heart[-trainIndex_heart]))))
  #print(paste('RBF HEART Random - 30 centros - random', as.character(acuracia(yHatTreinamento_heart_random, y_heart[-trainIndex_heart]))))
  
  ##############################
  
  acuracia_elm_bcw <- c(acuracia_elm_bcw, acuracia(yELM_bcw, y_bcw[-trainIndex_bcw]))
  acuracia_bcw <- c(acuracia_bcw, acuracia(yHatTreinamento_bcw, y_heart[-trainIndex_heart]))
  acuracia_bcw_random <- c(acuracia_bcw_random, acuracia(yHatTreinamento_bcw_random, y_heart[-trainIndex_heart]))
  
  acuracia_elm_heart <- c(acuracia_elm_heart, acuracia(yELM_heart, y_heart[-trainIndex_heart]))
  acuracia_heart <- c(acuracia_heart, acuracia(yHatTreinamento_heart, y_heart[-trainIndex_heart]))
  acuracia_heart_random <- c(acuracia_heart_random, acuracia(yHatTreinamento_heart_random, y_heart[-trainIndex_heart]))
}

acuracia_media_elm_bcw <- mean(acuracia_elm_bcw)
acuracia_media_bcw <- mean(acuracia_bcw)
acuracia_media_bcw_random <- mean(acuracia_bcw_random)

acuracia_sd_elm_bcw <- sd(acuracia_elm_bcw)
acuracia_sd_bcw <- sd(acuracia_bcw)
acuracia_sd_bcw_random <- sd(acuracia_bcw_random)


acuracia_media_elm_heart <- mean(acuracia_elm_heart)
acuracia_media_heart <- mean(acuracia_heart)
acuracia_media_heart_random <- mean(acuracia_heart_random)

acuracia_sd_elm_heart <- sd(acuracia_elm_heart)
acuracia_sd_heart <- sd(acuracia_heart)
acuracia_sd_heart_random <- sd(acuracia_heart_random)

print('Acurácias Médias')
print(paste('ELM BCW - 300 neurônios.', as.character(acuracia_media_elm_bcw), '+-', as.character(acuracia_sd_elm_bcw)))
print(paste('RBF BCW - 20 centros', as.character(acuracia_media_bcw), '+-', as.character(acuracia_sd_bcw)))
print(paste('RBF BCW Random - 30 centros - random', as.character(acuracia_media_bcw_random), '+-', as.character(acuracia_sd_bcw_random)))

##############################

print(paste('ELM HEART - 300 neurônios.', as.character(acuracia_media_elm_heart), '+-', as.character(acuracia_sd_elm_heart)))
print(paste('RBF HEART - 20 centros', as.character(acuracia_media_heart), '+-', as.character(acuracia_sd_heart)))
print(paste('RBF HEART Random - 30 centros - random', as.character(acuracia_media_heart_random), '+-', as.character(acuracia_sd_heart_random)))
