
rm(list = ls())
library(mlbench)
library(e1071)
library(SparseM)
require(ks)
require(RSNNS)
require(nnfor)

getMatrix = function(fileName) {
  table = read.csv(fileName)

  dadosOpen = table$Open
  dadosClose = table$Close

  allData = cbind(dadosOpen, dadosClose)

  mediaData = rowMeans(allData)
  dadosEmMatriz = matrix(0, nrow = (length(mediaData) - 101), ncol = 101)

  ###########################################
  # Transformando os dados em matrizes

  for (i in 1:(length(mediaData) - 101)) {
    dadosEmMatriz[i,] = mediaData[i:(i + 100)]
    
    if(mediaData[i+100] > mediaData[i+99]) {
      dadosEmMatriz[i, 101] = 1
    } else {
      dadosEmMatriz[i, 101] = -1
    }
}

  return(dadosEmMatriz)
}

getDataframe = function(data) {  
  trainIndex <- createDataPartition(c(1:(nrow(data)-101)), p=0.7, list=FALSE)

  dadosTreinamento = data[trainIndex,]
  dadosTeste = data[-trainIndex,]

  list = list(treinamento = data.frame(dadosTreinamento[,1:100], y = dadosTreinamento[,101]), 
    teste = dadosTeste)
  return(list)
}

###########################################
# Adquirindo dados

tableEURUSD = getMatrix('EURUSD_D1.csv')
tableEURGBP = getMatrix('EURGBP_D1.csv')
tableUSDJPY = getMatrix('USDJPY_D1.csv')

###########################################
# Separando dados de treinamento e testes

listEURUSD = getDataframe(tableEURUSD) 
EURUSDtreinamento = listEURUSD$treinamento
EURUSDteste = listEURUSD$teste

listEURGBP = getDataframe(tableEURGBP) 
EURGBPtreinamento = listEURGBP$treinamento
EURGBPteste = listEURGBP$teste

listUSDJPY = getDataframe(tableUSDJPY) 
USDJPYtreinamento = listUSDJPY$treinamento
USDJPYteste = listUSDJPY$teste

###########################################
# Treinando Modelos

# RBF
rbfEURUSD = rbf(EURUSDtreinamento[,1:100], EURUSDtreinamento[,101])
rbfEURGBP = rbf(EURGBPtreinamento[,1:100], EURGBPtreinamento[,101])
rbfUSDJPY = rbf(USDJPYtreinamento[,1:100], USDJPYtreinamento[,101])

# MLP
mlpEURUSD = RSNNS::mlp(EURUSDtreinamento[,1:100], EURUSDtreinamento[,101])
mlpEURGBP = RSNNS::mlp(EURGBPtreinamento[,1:100], EURGBPtreinamento[,101])
mlpUSDJPY = RSNNS::mlp(USDJPYtreinamento[,1:100], USDJPYtreinamento[,101])

# ELM
elmEURUSD = nnfor::elm(ts(EURUSDtreinamento))
elmEURGBP = nnfor::elm(ts(EURGBPtreinamento))
elmUSDJPY = nnfor::elm(ts(USDJPYtreinamento))

###########################################
# Previsões

# RBF
RBFpredictEURUSD = predict(rbfEURUSD, EURUSDteste[,1:100])
RBFpredictEURGBP = predict(rbfEURGBP, EURGBPteste[,1:100])
RBFpredictUSDJPY = predict(rbfUSDJPY, USDJPYteste[,1:100])

# MLP
MLPpredictEURUSD = predict(mlpEURUSD, EURUSDteste[,1:100])
MLPpredictEURGBP = predict(mlpEURGBP, EURGBPteste[,1:100])
MLPpredictUSDJPY = predict(mlpUSDJPY, USDJPYteste[,1:100])

# ELM
ELMpredictEURUSD = forecast(elmEURUSD, y = EURUSDteste[,1:100])
ELMpredictEURGBP = forecast(elmEURGBP, y = EURGBPteste[,1:100])
ELMpredictUSDJPY = forecast(elmUSDJPY, y = USDJPYteste[,1:100])

###########################################
# Erro

# RBF
mseRBFEURUSD = (sum((RBFpredictEURUSD - EURUSDteste[,101])^2))/nrow(EURUSDteste)
mseRBFEURGBP = (sum((RBFpredictEURGBP - EURGBPteste[,101])^2))/nrow(EURGBPteste)
mseRBFUSDJPY = (sum((RBFpredictUSDJPY - USDJPYteste[,101])^2))/nrow(USDJPYteste)

# MLP
mseMLPEURUSD = (sum((MLPpredictEURUSD - EURUSDteste[,101])^2))/nrow(EURUSDteste)
mseMLPEURGBP = (sum((MLPpredictEURGBP - EURGBPteste[,101])^2))/nrow(EURGBPteste)
mseMLPUSDJPY = (sum((MLPpredictUSDJPY - USDJPYteste[,101])^2))/nrow(USDJPYteste)

# ELM
mseELMEURUSD = (sum((ELMpredictEURUSD - EURUSDteste[,101])^2))/nrow(EURUSDteste)
mseELMEURGBP = (sum((ELMpredictEURGBP - EURGBPteste[,101])^2))/nrow(EURGBPteste)
mseELMUSDJPY = (sum((ELMpredictUSDJPY - USDJPYteste[,101])^2))/nrow(USDJPYteste)