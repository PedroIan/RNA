from matplotlib import pyplot as plt
import array

t = [0.314159265358979, 0.628318530717959, 0.942477796076938, 1.256637061435,
     1.5707963267949, 1.88495559215388, 2.19911485751286, 2.51327412287183,
     2.82743338823081, 3.14159265358979, 3.45575191894877, 3.76991118430775,
     4.08407044966673, 4.39822971502571, 4.71238898038469, 5.02654824574367,
     5.34070751110265, 5.65486677646163, 5.96902604182061, 6.28318530717959]

x = [0.29552020666134, 0.564642473395035, 0.783326909627483, 0.932039085967226,
     0.997494986604054, 0.973847630878195, 0.863209366648874, 0.675463180551151,
     0.42737988023383, 0.141120008059868, -0.157745694143248, -0.442520443294852,
     -0.687766159183973, -0.871575772413588, -0.977530117665097, -0.996164608835841,
     -0.925814682327732, -0.772764487555988, -0.550685542597638, -0.279415498198926]

y = [0.588656061998402, 0.669392742018511, 0.734998072888245, 0.779611725790168,
     0.799248495981216, 0.792154289263459, 0.758962809994662, 0.702638954165345,
     0.628213964070149, 0.54233600241796, 0.452676291757026, 0.367243867011544,
     0.293670152244808, 0.238527268275924, 0.206740964700471, 0.201150617349248,
     0.22225559530168, 0.268170653733204, 0.334794337220709, 0.416175350540322]

treinamento = []
somenteXAxisTreinamento = []
somenteYAxisTreinamento = []

teste = []
somenteXAxisTeste = []
somenteYAxisTeste = []

somenteSaidaParaTreinamento = []
somenteSaidaParaTeste = []

for i in range(len(t)):
    if(i == 4 or i == 7 or i == 11 or i == 13 or i == 14 or i == 17):
        teste.append([t[i], x[i]])
        somenteXAxisTeste.append(t[i])
        somenteYAxisTeste.append(x[i])
        somenteSaidaParaTreinamento.append(y[i])
    else:
        treinamento.append([teste.append([t[i], x[i]])])
        somenteXAxisTreinamento.append(t[i])
        somenteYAxisTreinamento.append(x[i])
        somenteSaidaParaTeste.append(y[i])

plt.scatter(somenteXAxisTreinamento, somenteYAxisTreinamento)
plt.scatter(somenteXAxisTeste, somenteYAxisTeste)
plt.show()

a = 0
b = 0
peso = 0

for i in range(100):
    erro = 0

    for j in range(len(somenteXAxisTreinamento)):
        erro = somenteSaidaParaTreinamento[j] - somenteYAxisTreinamento[j]*peso