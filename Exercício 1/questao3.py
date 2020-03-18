# Autor: Pedro Ian Mota Moraes
# Data de Início: 11/03/2020

import numpy as np
import random
import matplotlib.pyplot as plt

def funcaoG(x):
    return (0.5 * np.square(x) + 3 * x + 10)

def ruidoGaussiano(x):
    return (x + np.random.normal(0, 4))

def createMatrixH(XVector, degree):
    result = []
    for number in XVector:
        temp = []
        for i in range(degree, 0, -1):
            temp.append(np.power(number, i))
        result.append(temp)
    return result


xAxesBase = np.linspace(-15, 10, 1000)

yReal = []
yResult = []

for x in xAxesBase:
    yReal.append(funcaoG(x))

sampled = random.sample(yReal, 10)

xNovo = []
yRealSampled = []


for sample in sampled:
    yResult.append(ruidoGaussiano(sample))
    xNovo.append(xAxesBase[yReal.index(sample)])
    yRealSampled.append(yReal[yReal.index(sample)])

Hs = []
Ws = []

for i in range(1, 8, 1):
    Hs.append(createMatrixH(xNovo, i))
    # print(Hs[i-1])
    Ws.append(np.dot(np.linalg.pinv(Hs[i - 1]), yResult))
    plt.scatter(xNovo, np.poly1d(Ws[i - 1])(xNovo))

plt.plot(xAxesBase, yReal)
plt.show()
print('----------------------------')
# print(yResult)
print(Ws)

E = np.subtract(yRealSampled, yResult)
# Erro = Y - Yhat
print(E)
Eq = np.sum(np.multiply(E, E), 1)

# plt.plot(xAxesBase, yReal)
# plt.scatter(xNovo, yResult)
# plt.show()
