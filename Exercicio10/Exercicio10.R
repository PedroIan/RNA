xtrain = seq(from = 0, to = 2 * pi, by = 0.15)
xtrain = xtrain + (runif(length(xtrain)) - 0.5)/5
ytrain = sin(xtrain)
ytrain = ytrain + (runif(length(xtrain)) - 0.5)/5
xtest = seq(from = 0, to = 2 * pi, by = 0.01)
ytest = sin(xtest)