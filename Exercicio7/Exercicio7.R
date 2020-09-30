
rm(list = ls())
source('treinaRBF.R')
library('corpcor')
library('plot3D')
library(mlbench)
library(caret)
library(ks)

for(i in c(10,20,40)) {
  rbfSinc(i)
  
}

for(i in c(5, 10, 30, 50)) {
  treinarTodasAsClasses(i)
  
}