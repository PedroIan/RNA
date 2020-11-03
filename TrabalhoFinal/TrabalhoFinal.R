
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
RBFpredictEURUSD[RBFpredictEURUSD > mean(RBFpredictEURUSD)] = 1
RBFpredictEURUSD[RBFpredictEURUSD < mean(RBFpredictEURUSD)] = -1

RBFpredictEURGBP = predict(rbfEURGBP, EURGBPteste[,1:100])
RBFpredictEURGBP[RBFpredictEURGBP > mean(RBFpredictEURGBP)] = 1
RBFpredictEURGBP[RBFpredictEURGBP < mean(RBFpredictEURGBP)] = -1

RBFpredictUSDJPY = predict(rbfUSDJPY, USDJPYteste[,1:100])
RBFpredictUSDJPY[RBFpredictUSDJPY > mean(RBFpredictUSDJPY)] = 1
RBFpredictUSDJPY[RBFpredictUSDJPY < mean(RBFpredictUSDJPY)] = -1

# MLP
MLPpredictEURUSD = predict(mlpEURUSD, EURUSDteste[,1:100])
MLPpredictEURUSD[MLPpredictEURUSD > mean(MLPpredictEURUSD)] = 1
MLPpredictEURUSD[MLPpredictEURUSD < mean(MLPpredictEURUSD)] = -1

MLPpredictEURGBP = predict(mlpEURGBP, EURGBPteste[,1:100])
MLPpredictEURGBP[MLPpredictEURGBP > mean(MLPpredictEURGBP)] = 1
MLPpredictEURGBP[MLPpredictEURGBP < mean(MLPpredictEURGBP)] = -1

MLPpredictUSDJPY = predict(mlpUSDJPY, USDJPYteste[,1:100])
MLPpredictUSDJPY[MLPpredictUSDJPY > mean(MLPpredictUSDJPY)] = 1
MLPpredictUSDJPY[MLPpredictUSDJPY < mean(MLPpredictUSDJPY)] = -1

# ELM
ELMpredictEURUSD = forecast(elmEURUSD, y = EURUSDteste[,1:100])
ELMpredictEURGBP = forecast(elmEURGBP, y = EURGBPteste[,1:100])
ELMpredictUSDJPY = forecast(elmUSDJPY, y = USDJPYteste[,1:100])


###########################################
# Erro

# RBF
caret::confusionMatrix(factor(EURUSDteste[,101]), factor(RBFpredictEURUSD))
caret::confusionMatrix(factor(EURGBPteste[,101]), factor(RBFpredictEURGBP))
caret::confusionMatrix(factor(USDJPYteste[,101]), factor(RBFpredictUSDJPY))

# MLP
caret::confusionMatrix(factor(EURUSDteste[,101]), factor(MLPpredictEURUSD))
caret::confusionMatrix(factor(EURGBPteste[,101]), factor(MLPpredictEURGBP))
caret::confusionMatrix(factor(USDJPYteste[,101]), factor(MLPpredictUSDJPY))

# ELM
caret::confusionMatrix(factor(EURUSDteste[,101]), factor(ELMpredictEURUSD))
caret::confusionMatrix(factor(EURGBPteste[,101]), factor(ELMpredictEURGBP))
caret::confusionMatrix(factor(USDJPYteste[,101]), factor(ELMpredictUSDJPY))