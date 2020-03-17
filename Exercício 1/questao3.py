import numpy as np
import matplotlib.pyplot as plt

# definir uma função geradora de grau 2, assim como passado no pdf

def funcaoG(x):
    return (0.5 * np.square(x) + 3 * x + 10)

def ruidoGaussiano(x):
    return (x + np.random.normal(0, 4))


xAxesBase = np.linspace(-15, 10, 20)

yReal = []
yResult = []
for x in xAxesBase:
    yReal.append(funcaoG(x))
    yResult.append(ruidoGaussiano(yReal[-1]))

# plt.plot(xAxesBase, yReal)
# plt.plot(xAxesBase, yResult)
# plt.show()