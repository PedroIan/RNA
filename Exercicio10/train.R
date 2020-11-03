

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
    yhat = tanh(O)
    
    erro = yAtual - yhat
    flinhaO = sech2(O)
    dO = erro * flinhaO
    
    Wminus = W[-4,]
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

plot(evec[1:nEpocas], type='l')