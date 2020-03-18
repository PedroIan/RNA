# Autor: Pedro Ian Mota Moraes
# Data de Início: 11/03/2020

import numpy as np
# pseudo-inversa de uma matriz --> numpy.linalg.pinv()


a = np.random.randn(9, 6)
B = np.linalg.pinv(a)
print(a)
print(B)

def getNormalDestributionMatrix(lin, col):
    result = []
    for i in range(lin):
        line = []
        for g in range(col):
            line.append(np.random.normal())
        result.append(line)
    return result


N = 10
n = 5
m = 3
p = 8


# Fazer a distribuição normal em colunas com as dimensões

X = getNormalDestributionMatrix(n, N)
Y = getNormalDestributionMatrix(N, m)
Z = getNormalDestributionMatrix(n, p)
W = getNormalDestributionMatrix(p, m)

print(len(X))

U = np.dot(X, Y)
H = len(U)
print(H)
# U = X * Z (operação matricial)
# H = size of U

O = np.dot(H, W)
print(len(O))
# O = H * W (operação matricial)

Yhat = np.tanh(O)
#Yhat = -1 * (len(O) >= 0)
# Yhat = -1* size of O >= 0

E = np.subtract(Y, Yhat)
# Erro = Y - Yhat

Eq = np.sum(np.multiply(E, E), 1)
# Eq = matrix(rowSums(E * E), ncol = 1) / m

print(Eq)

