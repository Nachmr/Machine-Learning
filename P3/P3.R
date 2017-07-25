path = "~/Copy/Informatica/Carrera/3º/2º Cuatrimestre/AA/Prácticas/P3"
setwd(path)
library(ISLR)
library(e1071)
library(tree)
library(ROCR)
library(randomForest)
library(gbm)


#################################################
#Ejercicio 1
#################################################
fix(OJ)
attach(OJ)

#Apartado 1.1
  set.seed(1)
  indices = sample (dim(OJ)[1], 800, replace=FALSE)
  train = OJ[indices, ]
  test = OJ[-indices, ]
  
  set.seed(1)
  svmfit = svm(Purchase ~ . , data = train, kernel ="linear" , cost =0.01 ,scale = FALSE )

#Apartado 1.2
  summary(svmfit)
  
  Purchase.train = train[, "Purchase"]
  Purchase.test = test[, "Purchase"]
    
  #Error Train
  set.seed(1)
  svm.pred = predict (svmfit, train)
  table(svm.pred, Purchase.train)
  acierto.train = mean(svm.pred == Purchase.train) #Este es el acierto
  error.train = 1 - acierto.train #Este el error
  error.train

  #Error Test
  set.seed(1)
  svm.pred = predict (svmfit, test)
  table(svm.pred, Purchase.test)
  acierto.test = mean(svm.pred == Purchase.test) #Acierto
  error.test = 1- acierto.test #Error
  error.test

#Apartado 1.3

  rocplot = function ( pred , truth , ...) {
      predob = prediction(pred, truth)
      perf = performance(predob, "tpr", "fpr")
      plot (perf ,...) 
      }
  
  #ROC para cost =0.001
  set.seed(1)
  svmfit.opt = svm(Purchase~. , data = train, kernel ="linear", cost =0.001, decision.values = T)
  fitted = attributes(predict(svmfit.opt, train, decision.values = TRUE))$decision.values
  rocplot(fitted, Purchase.train, main ="Training Data")
  
  #ROC para cost =0.01
  set.seed(1)
  svmfit.opt = svm(Purchase~. , data = train, kernel ="linear", cost =0.01, decision.values = T)
  fitted = attributes(predict(svmfit.opt, train, decision.values = TRUE))$decision.values
  rocplot(fitted, Purchase.train, main ="Training Data")
  
  #ROC para cost =0.1
  set.seed(1)
  svmfit.opt = svm(Purchase~. , data = train, kernel ="linear", cost =0.1, decision.values = T)
  fitted = attributes(predict(svmfit.opt, train, decision.values = TRUE))$decision.values
  rocplot(fitted, Purchase.train, main ="Training Data")
  
  #ROC para cost =1
  set.seed(1)
  svmfit.opt = svm(Purchase~. , data = train, kernel ="linear", cost =1, decision.values = T)
  fitted = attributes(predict(svmfit.opt, train, decision.values = TRUE))$decision.values
  rocplot(fitted, Purchase.train, main ="Training Data")
  
  #ROC para cost =10
  set.seed(1)
  svmfit.opt = svm(Purchase~. , data = train, kernel ="linear", cost =10, decision.values = T)
  fitted = attributes(predict(svmfit.opt, train, decision.values = TRUE))$decision.values
  rocplot(fitted, Purchase.train, main ="Training Data")
  
  #Cost óptimo
  set.seed(1)
  tune.out = tune(svm, Purchase~. , data = train, kernel = "linear",ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10)))
  tune.out

#Apartado 1.4
  set.seed(1)
  svmfit = svm(Purchase~. , data = train, kernel ="linear", cost =0.01, decision.values = T)
  
  #Error Train
  set.seed(1)
  svm.pred = predict (svmfit, train)
  table(svm.pred, Purchase.train)
  acierto.train = mean(svm.pred == Purchase.train) #Este es el acierto
  error.train = 1 - acierto.train #Este el error
  error.train
  
  #Error Test
  set.seed(1)
  svm.pred = predict (svmfit, test)
  table(svm.pred, Purchase.test)
  acierto.test = mean(svm.pred == Purchase.test) #Acierto
  error.test = 1- acierto.test #Error
  error.test

#Aparatado 1.5
  #svm radial kernel
  set.seed(1)
  svmfit.rad = svm(Purchase~. , data = train, kernel ="radial", gamma =10, decision.values = T)
  summary(svmfit.rad)
  
  #Error Train
  set.seed(1)
  svmrad.pred = predict (svmfit.rad, train)
  table(svmrad.pred, Purchase.train)
  acierto.train = mean(svmrad.pred == Purchase.train) #Este es el acierto
  error.train = 1 - acierto.train #Este el error
  error.train
  
  #Error Test
  set.seed(1)
  svmrad.pred = predict (svmfit.rad, test)
  table(svmrad.pred, Purchase.test)
  acierto.test = mean(svmrad.pred == Purchase.test) #Este es el acierto
  error.test = 1 - acierto.test #Este el error
  error.test
  
  #Curva ROC para gamma = 10
  set.seed(1)
  svmfit.rad.opt = svm(Purchase~. , data = train, kernel ="radial", gamma =10, decision.values = T)
  fitted = attributes(predict(svmfit.rad.opt, train, decision.values = TRUE))$decision.values
  rocplot(fitted, Purchase.train, main ="Training Data")
  
  #Curva ROC para gamma = 1
  set.seed(1)
  svmfit.rad.opt = svm(Purchase~. , data = train, kernel ="radial", gamma =1, decision.values = T)
  fitted = attributes(predict(svmfit.rad.opt, train, decision.values = TRUE))$decision.values
  rocplot(fitted, Purchase.train, main ="Training Data")
  
  #Curva ROC para gamma = 0.1
  set.seed(1)
  svmfit.rad.opt = svm(Purchase~. , data = train, kernel ="radial", gamma =0.1, decision.values = T)
  fitted = attributes(predict(svmfit.rad.opt, train, decision.values = TRUE))$decision.values
  rocplot(fitted, Purchase.train, main ="Training Data")
  
  #Curva ROC para gamma = 0.01
  set.seed(1)
  svmfit.rad.opt = svm(Purchase~. , data = train, kernel ="radial", gamma =0.01, decision.values = T)
  fitted = attributes(predict(svmfit.rad.opt, train, decision.values = TRUE))$decision.values
  rocplot(fitted, Purchase.train, main ="Training Data")
  
  #Curva ROC para gamma = 0.001
  set.seed(1)
  svmfit.rad.opt = svm(Purchase~. , data = train, kernel ="radial", gamma =0.001, decision.values = T)
  fitted = attributes(predict(svmfit.rad.opt, train, decision.values = TRUE))$decision.values
  rocplot(fitted, Purchase.train, main ="Training Data")
  
  #Calculamos el cost óptimo
  set.seed(1)
  tune.out = tune(svm, Purchase~. , data = train, kernel = "radial",ranges = list(gamma = c(10, 1, 0.1, 0.01, 0.001)))
  tune.out
  
  #Calculamos los errores de train y test
  set.seed(1)
  svmfit.rad.opt = svm(Purchase~. , data = train, kernel ="radial", gamma =0.01, cost=1, decision.values = T)
  
  #Error Train
  set.seed(1)
  svmfit.rad.opt.pred = predict (svmfit, train)
  table(svmfit.rad.opt.pred, Purchase.train)
  acierto.train = mean(svmfit.rad.opt.pred == Purchase.train) #Este es el acierto
  error.train = 1 - acierto.train #Este el error
  error.train
  
  #Error Test
  set.seed(1)
  svmfit.rad.opt.pred = predict (svmfit, test)
  table(svmfit.rad.opt.pred, Purchase.test)
  acierto.test = mean(svmfit.rad.opt.pred == Purchase.test) #Este es el acierto
  error.test = 1 - acierto.test #Este el error
  error.test

#Apartado 1.6
  #svm con nucleo polinómico
  set.seed(1)
  svmfit.pol = svm(Purchase~. , data = train, kernel ="polynomial", degree = 2, decision.values = T)
  summary(svmfit.pol)
  
  #Error Train
  set.seed(1)
  svmpol.pred = predict (svmfit.pol, train)
  table(svmpol.pred, Purchase.train)
  acierto.train = mean(svmpol.pred == Purchase.train) #Este es el acierto
  error.train = 1 - acierto.train #Este el error
  error.train
  
  #Error Test
  set.seed(1)
  svmpol.pred = predict (svmfit.pol, test)
  table(svmpol.pred, Purchase.test)
  acierto.test = mean(svmpol.pred == Purchase.test) #Este es el acierto
  error.test = 1 - acierto.test #Este el error
  error.test
  
  #Curva ROC para degree = 2
  set.seed(1)
  svmfit.pol = svm(Purchase~. , data = train, kernel ="polynomial", degree = 2, decision.values = T)
  fitted = attributes(predict(svmfit.pol, train, decision.values = TRUE))$decision.values
  rocplot(fitted, Purchase.train, main ="Training Data")
  
  #Curva ROC para degree = 3
  set.seed(1)
  svmfit.pol = svm(Purchase~. , data = train, kernel ="polynomial", degree = 3, decision.values = T)
  fitted = attributes(predict(svmfit.pol, train, decision.values = TRUE))$decision.values
  rocplot(fitted, Purchase.train, main ="Training Data")
  
  #Curva ROC para degree = 4
  set.seed(1)
  svmfit.pol = svm(Purchase~. , data = train, kernel ="polynomial", degree = 4, decision.values = T)
  fitted = attributes(predict(svmfit.pol, train, decision.values = TRUE))$decision.values
  rocplot(fitted, Purchase.train, main ="Training Data")
  
  #Curva ROC para degree = 5
  set.seed(1)
  svmfit.pol = svm(Purchase~. , data = train, kernel ="polynomial", degree = 5, decision.values = T)
  fitted = attributes(predict(svmfit.pol, train, decision.values = TRUE))$decision.values
  rocplot(fitted, Purchase.train, main ="Training Data")
  
  #Curva ROC para degree = 6
  set.seed(1)
  svmfit.pol = svm(Purchase~. , data = train, kernel ="polynomial", degree = 6, decision.values = T)
  fitted = attributes(predict(svmfit.pol, train, decision.values = TRUE))$decision.values
  rocplot(fitted, Purchase.train, main ="Training Data")
  
  #Calculamos el cost óptimo
  set.seed(1)
  tune.out = tune(svm, Purchase~. , data = train, kernel = "polynomial",ranges = list(degree = c(2,3,4,5,6)))
  tune.out
  
  #Calculamos los errores de train y test
  set.seed(1)
  svmfit.pol.opt = svm(Purchase~. , data = train, kernel ="radial", degree = 3, decision.values = T)
  
  #Error Train
  set.seed(1)
  svmfit.pol.opt.pred = predict (svmfit.pol.opt, train)
  table(svmfit.pol.opt.pred, Purchase.train)
  acierto.train = mean(svmfit.pol.opt.pred == Purchase.train) #Este es el acierto
  error.train = 1 - acierto.train #Este el error
  error.train
  
  #Error Test
  set.seed(1)
  svmfit.pol.opt.pred = predict (svmfit.pol.opt, test)
  table(svmfit.pol.opt.pred, Purchase.test)
  acierto.test = mean(svmfit.pol.opt.pred == Purchase.test) #Este es el acierto
  error.test = 1 - acierto.test #Este el error
  error.test





#################################################
# Ejercicio 2
#################################################

#Apartado 2.1

  #usaré las mismas muestras que en el ejercico 1 para train y test
  set.seed(1)
  
  tree.fit = tree(Purchase~., data = train)
  tree.fit
  
  plot(tree.fit)

#Apartado 2.2
  summary(tree.fit)
  tree.fit


#Apartado 2.3
  #Dibujo del arbol
  plot(tree.fit)
  tree.fit
  
  #Mejores 4
  set.seed(1)
  best4 = prune.misclass(tree.fit, best = 4)
  best4

#Apartado 2.4
  set.seed(1)
  tree.pred = predict(tree.fit, test, type ="class")
  table(tree.pred, Purchase.test)
  209/270
  acierto.test = mean(tree.pred == Purchase.test) #Este es el acierto
  acierto.test
  error.test = 1 - acierto.test #Este el error
  error.test

#Apartado 2.5
  set.seed(1)
  cv.tree=cv.tree(tree.fit)
  cv.tree

#Apartado 2.6
  plot(cv.tree$size, cv.tree$dev)

#Apartado 2.7
  set.seed(1)
  prune.tree = prune.misclass(tree.fit , best = 4)
  
  #Error de train del árbol sin podar
  set.seed(1)
  tree.pred = predict(tree.fit, train, type ="class")
  table(tree.pred, Purchase.train)
  acierto.train = mean(tree.pred == Purchase.train) #Este es el acierto
  error.train = 1 - acierto.train #Este el error
  error.train
  
  #Error de test del arbol sin podar
  set.seed(1)
  tree.pred = predict(tree.fit, test, type ="class")
  table(tree.pred, Purchase.test)
  acierto.test = mean(tree.pred == Purchase.test) #Este es el acierto
  error.test = 1 - acierto.test #Este el error
  error.test
  
  #Error de train del árbol podado
  set.seed(1)
  tree.pred = predict(prune.tree, train, type ="class")
  table(tree.pred, Purchase.train)
  acierto.train = mean(tree.pred == Purchase.train) #Este es el acierto
  error.train = 1 - acierto.train #Este el error
  error.train
  
  #Error de test del arbol podado
  set.seed(1)
  tree.pred = predict(prune.tree, test, type ="class")
  table(tree.pred, Purchase.test)
  acierto.test = mean(tree.pred == Purchase.test) #Este es el acierto
  error.test = 1 - acierto.test #Este el error
  error.test




#################################################
#Ejercicio 3
#################################################

fix(Hitters)
attach(Hitters)

#Apartado 3.1
  Hitters=na.omit(Hitters)

  Hitters[, "Salary"] = log(Hitters[, "Salary"])
  
  set.seed(1)
  indices = sample (dim(Hitters)[1], 200, replace=FALSE)
  train = Hitters[indices, ]
  test = Hitters[-indices, ]

#Apartado 3.2
  set.seed(1)
  
  Salary.train = train[,"Salary"]
  Salary.test = test[,"Salary"]
  
  MSE = rep(0, 7)
  landa = c(0.001, 0.005, 0.01, 0.05, 0.5, 05, 1)

  #train
  set.seed(1)
  indice = 1
  for(i in landa){
    boosting.fit = gbm(Salary~. , data = train, shrinkage = i , n.trees =1000, distribution = "gaussian", verbose = F, interaction.depth =4)
    boosting.pred = predict(boosting.fit, newdata = train, n.trees = 1000)
    MSE[indice] = mean((boosting.pred - Salary.train)^2)
    indice = indice+1
  }
  
  plot(landa, MSE, main = "Train")

#Apartado 3.3
  set.seed(1)
  
  #Boosting test
  indice = 1
  for(i in landa){
    boosting.fit = gbm(Salary~. , data = test, shrinkage = i , n.trees =1000, distribution = "gaussian", verbose = F, interaction.depth =4)
    boosting.pred = predict(boosting.fit, newdata = test, n.trees = 1000)
    MSE[indice] = mean((boosting.pred - Salary.test)^2)
    indice = indice+1
  }
  
  plot(landa, MSE, main = "Test")
  
  #Regresión múltiple
  set.seed(1)
  reg.mul = lm(Salary~ . , data = test)
  reg.mul.pred = predict(reg.mul, test)
  MSE.reg.mul = mean((reg.mul.pred - Salary.test)^2)
  MSE.reg.mul
  
  #Media de MSE de Boosting
  mean.MSE = mean(MSE)
  mean.MSE

#Apartado 3.4

  summary(boosting.fit)

#Apartado 3.5
  
  set.seed(1)
  bag.fit = randomForest(Salary~. , data = train)
  bag.pred = predict(bag.fit, test)
  MSE = mean((bag.pred - Salary.test)^2)
  MSE

