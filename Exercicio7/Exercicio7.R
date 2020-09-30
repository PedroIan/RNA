
rm(list = ls())
source('treinaRBF.R')
library('corpcor')
library('plot3D')
library(mlbench)
library(caret)
library(ks)

cores <- rainbow(6)

dnormals <- mlbench.2dnormals(200)
xor <- mlbench.xor(100)
circle <- mlbench.circle(100)
spirals <- mlbench.spirals(100,sd = 0.05)

allEntrada <- list(dnormals, xor, circle, spirals)

rbfSinc(40)
