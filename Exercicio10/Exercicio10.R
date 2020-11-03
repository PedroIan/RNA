
rm(list = ls())
library(mlbench)
library(e1071)
library(SparseM)
require(caret)
require(ks)

sech2 = function(u) {
    return (((2/(exp(u)+exp(-u)))*(2/(exp(u)+exp(-u)))))
}

xtrain = seq(from = 0, to = 2 * pi, by = 0.15)
xtrain = xtrain + (runif(length(xtrain)) - 0.5)/5
ytrain = sin(xtrain)
ytrain = ytrain + (runif(length(xtrain)) - 0.5)/5
xtest = seq(from = 0, to = 2 * pi, by = 0.01)
ytest = sin(xtest)

# Inicialização dos pesos (arbitrariamente)



tolerancia = 0.001
eta = 0.001
maxEpocas = 2000
nEpocas = 0
erroEpoca = tolerancia + 1
N = length(xtrain)
evec = matrix(nrow = maxEpocas, ncol = 1)

xAtual = matrix(1, nrow = 2, ncol = 1)
errosVector = vector()
for (j in 1:5) {
  Z = matrix(rnorm(6) - 0.5, ncol = 3, nrow = 2)
  W = matrix(rnorm(4) - 0.5, ncol = 1, nrow = 4)
  
  while((nEpocas < maxEpocas) && (erroEpoca > tolerancia)) {
      ei2 = 0
      xseq = sample(N)
      for(i in 1:N) {
          irand = xseq[i]
          xAtual[1,1] = xtrain[irand]
  
          yAtual = ytrain[irand]
  
          U = t(xAtual)%*%Z
          H = tanh(U)
          Haug = cbind(H, 1)
  
          O = Haug %*% W
          yhat = O
  
          erro = yAtual - yhat
          flinhaO = sech2(O)
          dO = erro * flinhaO
  
          Wminus = W[-3,]
          eHidden = dO %*% t(Wminus)
          flinhaU = sech2(U)
          dU = eHidden * flinhaU
  
          W = W + eta*(t(Haug) %*% dO)
          Z = Z + eta*(xAtual %*% dU)
  
          ei2 = ei2 + (erro %*% t(erro))
      }
  
      nEpocas = nEpocas + 1
      erroEpoca = ei2 / N
      evec[nEpocas] = erroEpoca
  }
  
  teste = cbind(xtest, 1)
  Hteste = cbind(tanh(teste%*%Z), 1)
  ytesteHat = Hteste %*% W
  
  errosVector[j] = sum((ytest - ytesteHat)^2)/length(ytest)
}
errosVector
MSE = mean(errosVector)
SD = sd(errosVector)

erroQuadraticoMedio = sum(evec[1:nEpocas]^2)/nEpocas

plot(xtest, ytest, col='blue', type='l', ylim=c(-1,1), xlim = c(0,6), ylab = 'Teste')
par(new=T)
plot(xtest, ytesteHat, col='red', ylim=c(-1,1), xlim = c(0,6), ylab = 'Treinamento')

#plot(evec[1:nEpocas], type='l')