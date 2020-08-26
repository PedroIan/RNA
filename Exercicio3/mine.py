from matplotlib import pyplot as plt
import numpy as np
import random
import array


def trainAdaline(Xin, Yout, pesoAtualizacao, tolerancia, maxInteracao):
    nLinhas = len(Xin)
    nColunas = len(Xin[0])

    print('Xin')
    print(Xin)

    vetorPesos = [random.uniform(-10, 10) for i in range(nColunas)]
    sequencia = [i for i in range(nLinhas)]
    erroPorInteracao = 1000
    vetorErros = []
    erroQuadratico = 0

    for i in range(maxInteracao):

        if (erroPorInteracao < tolerancia):
            break

        random.shuffle(sequencia)

        for j in range(nLinhas - 1):
            ponto = sequencia[j]

            erro = Yout[ponto] - np.dot(vetorPesos, Xin[j])

            vetorPesos = vetorPesos + pesoAtualizacao * erro * Xin[j]

            erroQuadratico = erroQuadratico + pow(erro, 2)

        vetorErros.append(erroQuadratico/nLinhas)

        erroPorInteracao = erroQuadratico/nLinhas

    return np.dot(vetorPesos, 1)


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
    if(i == 4 or i ==  7 or i ==  11 or i ==  13 or i ==  14 or i ==  17):
        teste.append([t[i], x[i]])
        somenteXAxisTeste.append([t[i]])
        somenteYAxisTeste.append([x[i]])
        somenteSaidaParaTeste.append([y[i]])
    else:
        treinamento.append([t[i], x[i]])
        somenteXAxisTreinamento.append([t[i]])
        somenteYAxisTreinamento.append([x[i]])
        somenteSaidaParaTreinamento.append([y[i]])

# plt.scatter(somenteXAxisTreinamento, somenteYAxisTreinamento)
# plt.scatter(somenteXAxisTeste, somenteYAxisTeste)
# plt.show()

pesos = trainAdaline(somenteYAxisTreinamento,
                     somenteSaidaParaTreinamento, 0.02, 0.05, 100)
print(pesos)

novoYSaida = np.dot(somenteYAxisTeste, pesos)
novoTodaSaida = np.dot(somenteYAxisTreinamento, pesos)

plt.scatter(somenteXAxisTreinamento,
            somenteYAxisTreinamento, label='Treinamento')
plt.scatter(somenteXAxisTeste, novoYSaida, label='Saída Treinada')
plt.scatter(somenteXAxisTeste, somenteYAxisTeste, label='Teste')
plt.scatter(somenteXAxisTreinamento, novoTodaSaida,
            label='Saída Completa Atualizada')
plt.legend()
plt.show()


X3Funcoes = [
    [0.425950531568867, 1.31094093866055, -1.60579308398418],
    [0.81020605733591, 1.11515296917338, -1.43676223303848],
    [1.11515296917338, 0.81020605733591, -1.26773138209277],
    [1.31094093866055, 0.425950531568867, -1.09870053114707],
    [1.37840487520902, pow(1.38756813824575, -16), -0.929669680201368],
    [1.31094093866055, -0.425950531568866, -0.760638829255665],
    [1.11515296917338, -0.81020605733591, -0.591607978309962],
    [0.81020605733591, -1.11515296917338, -0.422577127364258],
    [0.425950531568867, -1.31094093866055, -0.253546276418555],
    [pow(1.7725769469902, -16), -1.37840487520902, -0.0845154254728517],
    [-0.425950531568866, -1.31094093866055, 0.0845154254728517],
    [-0.81020605733591, -1.11515296917338, 0.253546276418555],
    [-1.11515296917338, -0.81020605733591, 0.422577127364258],
    [-1.31094093866055, -0.425950531568867, 0.591607978309962],
    [-1.37840487520902, pow(-1.98855009846192, -16), 0.760638829255665],
    [-1.31094093866055, 0.425950531568866, 0.929669680201368],
    [-1.11515296917338, 0.81020605733591, 1.09870053114707],
    [-0.81020605733591, 1.11515296917338, 1.26773138209277],
    [-0.425950531568867, 1.31094093866055, 1.43676223303848],
    [pow(-3.2916004080713, -16), 1.37840487520902, 1.60579308398418]
]

novaBase = [0.314159265358979, 0.628318530717959, 0.942477796076938, 1.25663706143592,
            1.5707963267949, 1.88495559215388, 2.19911485751286, 2.51327412287183,
            2.82743338823081, 3.14159265358979, 3.45575191894877, 3.76991118430775,
            4.08407044966673, 4.39822971502571, 4.71238898038469, 5.02654824574367,
            5.34070751110265, 5.65486677646163, 5.96902604182061, 6.28318530717959
            ]

Y3 = [-0.198750516267684, 0.301021623362128, 0.503167264361769, 0.437536735151964,
      0.160192161399815, -0.252080285449282, -0.70928675363343, -1.11703493630872,
      -1.385773848213, -1.4395597000417, -1.22348980567651, -0.709076839632104,
      0.102962625042473, 1.1827782599265, 2.47430793935287, 3.90076549187618,
      5.37215706573455, 6.79409035408407, 8.07701437166256, 9.14498532916549, ]

treinamento = []
somenteXAxisTreinamento = []
somenteYAxisTreinamento = []

teste = []
somenteXAxisTeste = []
somenteYAxisTeste = []

somenteSaidaParaTreinamento = []
somenteSaidaParaTeste = []

for i in range(len(novaBase)):
    if(i == 0 or i == 2 or i == 4 or i == 5 or i == 6 or i == 7 or i == 8 or i == 10 or i == 11 or i == 12 or i == 14 or i == 15 or i == 16 or i == 19):
        treinamento.append([novaBase[i], X3Funcoes[i]])
        somenteXAxisTreinamento.append([novaBase[i]])
        somenteYAxisTreinamento.append(X3Funcoes[i])
        somenteSaidaParaTreinamento.append([Y3[i]])
    else:
        teste.append([novaBase[i], X3Funcoes[i]])
        somenteXAxisTeste.append([novaBase[i]])
        somenteYAxisTeste.append(X3Funcoes[i])
        somenteSaidaParaTeste.append([Y3[i]])

pesos = trainAdaline(somenteYAxisTreinamento,
                     somenteSaidaParaTreinamento, 0.02, 0.05, 100)
print(pesos)

novoYSaida = np.dot(somenteYAxisTeste, pesos)
novoTodaSaida = np.dot(somenteYAxisTreinamento, pesos)

plt.scatter(somenteXAxisTreinamento,
            somenteSaidaParaTreinamento, label='Treinamento')
plt.scatter(somenteXAxisTeste, novoYSaida, label='Saída Treinada')
plt.scatter(somenteXAxisTeste, somenteSaidaParaTeste, label='Teste')
plt.scatter(somenteXAxisTreinamento, novoTodaSaida,
            label='Saída Completa Atualizada')
plt.legend()
plt.show()