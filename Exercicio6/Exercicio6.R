
rm(list = ls())
library('plot3D')
library(caret)
library(ks)
source('treinaELM.R')
source('trainperceptron.R')
source('yperceptron.R')
source('readData.R')

for(nNeuronios in c(5, 10, 30, 50, 100, 300)){

  wdbc <- as.matrix(read.table('wdbc.data', header = FALSE, sep = ",", skip = 0))

  retlist_xy_wdbc <- readTable(wdbc, (3:32), 2, 'M')
  
  x_wdbc <- retlist_xy_wdbc[[1]]
  y_wdbc <- retlist_xy_wdbc[[2]]
  
  trainIndex_wdbc <- createDataPartition(y_wdbc, p=0.8, list=FALSE)
  
  retlist_ELM_wdpc <- treinaELM(x_wdbc[trainIndex_wdbc,], y_wdbc[trainIndex_wdbc], nNeuronios, 1)
  
  W_wdbc <- retlist_ELM_wdpc[[1]]
  H_wdbc <- retlist_ELM_wdpc[[2]]
  Z_wdbc <- retlist_ELM_wdpc[[3]]
  
  #retlist_Perceprton_wdpc <- trainperceptron(x_wdbc[trainIndex_wdbc,], y_wdbc[trainIndex_wdbc], 0.05, 0.002, 100, 0)
  
  #W_Perceprton_wdpc <- retlist_Perceprton_wdpc[[1]]
  
  yELM_wdbc <- YELM(x_wdbc[-trainIndex_wdbc,], Z_wdbc, W_wdbc, 1)
  #y_Perceprton_wdpc <- yperceptron(x_wdbc[-trainIndex_wdbc,], W_Perceprton_wdpc, 0)
  
  print(paste('Acurácia ELM WDBC - ', as.character(nNeuronios), ' neurônios.', as.character(acuracia(yELM_wdbc, y_wdbc[-trainIndex_wdbc]))))
  #print(paste('Acurácia Perceptron WDBC - ', as.character(acuracia(y_Perceprton_wdpc, y_wdbc[-trainIndex_wdbc]))))
  
  ##############################
  
  wpbc <- as.matrix(read.table('wpbc.data', header = FALSE, sep = ",", skip = 0))
  
  retlist_xy_wpbc <- readTable(wpbc, (4:34), 2, 'N')
  
  x_wpbc <- retlist_xy_wpbc[[1]]
  y_wpbc <- retlist_xy_wpbc[[2]]
  
  trainIndex_wpbc <- createDataPartition(y_wpbc, p=0.8, list=FALSE)
  
  retlist_ELM_wpbc <- treinaELM(x_wpbc[trainIndex_wpbc,], y_wpbc[trainIndex_wpbc], nNeuronios, 1)
  
  W_wpbc <- retlist_ELM_wpbc[[1]]
  H_wpbc <- retlist_ELM_wpbc[[2]]
  Z_wpbc <- retlist_ELM_wpbc[[3]]
  
  #retlist_Perceprton_wpbc <- trainperceptron(x_wpbc[trainIndex_wpbc,], y_wpbc[trainIndex_wpbc], 0.05, 0.002, 100, 0)
  
  #W_Perceprton_wpbc <- retlist_Perceprton_wpbc[[1]]
  
  yELM_wpbc <- YELM(x_wpbc[-trainIndex_wpbc,], Z_wpbc, W_wpbc, 1)
  #y_Perceprton_wpbc <- yperceptron(x_wpbc[-trainIndex_wpbc,], W_Perceprton_wpbc, 0)
  
  print(paste('Acurácia ELM WPBC - ', as.character(nNeuronios), ' neurônios.', as.character(acuracia(yELM_wpbc, y_wpbc[-trainIndex_wpbc]))))
  #print(paste('Acurácia Perceptron WPBC - ', as.character(acuracia(y_Perceprton_wpbc, y_wpbc[-trainIndex_wpbc]))))
  
  ##############################
  
  bcw  <- as.matrix(read.table('breast-cancer-wisconsin.data',
                               header = FALSE, sep = ",", skip = 0))
  
  retlist_xy_bcw <- readTable(bcw, (2:10), 11, 4)
  
  x_bcw <- retlist_xy_bcw[[1]]
  y_bcw <- retlist_xy_bcw[[2]]
  
  trainIndex_bcw <- createDataPartition(y_bcw, p=0.8, list=FALSE)
  
  retlist_ELM_bcw <- treinaELM(x_bcw[trainIndex_bcw,], y_bcw[trainIndex_bcw], nNeuronios, 1)
  
  W_bcw <- retlist_ELM_bcw[[1]]
  H_bcw <- retlist_ELM_bcw[[2]]
  Z_bcw <- retlist_ELM_bcw[[3]]
  
  #retlist_Perceprton_bcw <- trainperceptron(x_bcw[trainIndex_bcw,], y_wpbc[trainIndex_bcw], 0.05, 0.002, 100, 0)
  
  #W_Perceprton_bcw <- retlist_Perceprton_bcw[[1]]
  
  yELM_bcw <- YELM(x_bcw[-trainIndex_bcw,], Z_bcw, W_bcw, 1)
  #y_Perceprton_bcw <- yperceptron(x_bcw[-trainIndex_bcw,], W_Perceprton_bcw, 0)
  
  print(paste('Acurácia ELM BCW - ', as.character(nNeuronios), ' neurônios.', as.character(acuracia(yELM_bcw, y_bcw[-trainIndex_bcw]))))
  #print(paste('Acurácia Perceptron BCW - ', as.character(acuracia(y_Perceprton_bcw, y_bcw[-trainIndex_bcw]))))
  
  ##############################
  
  heart <- as.matrix(read.table('heart.dat', header = FALSE, sep = " ", skip = 0))
  
  retlist_xy_heart <- readTable(heart, (1:13), 14, 2)
  
  x_heart <- retlist_xy_heart[[1]]
  y_heart <- retlist_xy_heart[[2]]
  
  trainIndex_heart <- createDataPartition(y_heart, p=0.8, list=FALSE)
  
  retlist_ELM_heart <- treinaELM(x_heart[trainIndex_heart,], y_heart[trainIndex_heart], nNeuronios, 1)
  
  W <- retlist_ELM_heart[[1]]
  H <- retlist_ELM_heart[[2]]
  Z <- retlist_ELM_heart[[3]]
  
  #retlist_Perceprton_heart <- trainperceptron(x_heart[trainIndex_heart,], y_wpbc[trainIndex_heart], 0.05, 0.002, 100, 0)
  
  #W_Perceprton_heart <- retlist_Perceprton_heart[[1]]
  
  yELM_heart <- YELM(x_bcw[-trainIndex_heart,], Z_bcw, W_bcw, 1)
  #y_Perceprton_heart <- yperceptron(x_heart[-trainIndex_heart,], W_Perceprton_heart, 0)
  
  print(paste('Acurácia ELM HEART - ', as.character(nNeuronios), ' neurônios.', as.character(acuracia(yELM_heart, y_heart[-trainIndex_heart]))))
  #print(paste('Acurácia Perceptron HEART - ', as.character(acuracia(y_Perceprton_heart, y_heart[-trainIndex_heart]))))
}
