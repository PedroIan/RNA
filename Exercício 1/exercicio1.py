import numpy as np
import matplotlib.pyplot as plt

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

x = np.linspace(-1,1,20)
y = np.linspace(-1,1,20)
z = (x**2 + y**2)

def circleFunc(x, y):
    return np.sqrt(x**2 + y**2)

grid = np.meshgrid(x, y)
raio = 0.6

classe = 1*(circleFunc(grid[0], grid[1]) > raio)
# print(classe)

# plt.scatter(grid[0], grid[1], c = classe)  
# plt.show()

ax.scatter(grid[0], grid[1], z)