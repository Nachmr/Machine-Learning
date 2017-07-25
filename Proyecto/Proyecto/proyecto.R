#Autores:
  # Ignacio Martín Requena
  # Pedro Antonio Ruiz Cuesta

#Dataset utilizado:
  # Airfoil self-noise

#Atributos (6):
  #- Frequency, in Hertz
  #- Angle of attack, in degrees.
  #- Chord length, in meters.
  #- Free-stream velocity, in meters per second.
  #- Suction side displacement thickness, in meters.
  #- Scaled sound pressure level, in decibels.

#Directorio de trabajo
path = "~/Copy/Informatica/Carrera/3º/2º Cuatrimestre/AA/Proyecto/Proyecto"
setwd(path)

#Lectura de dataset
Airfoil=read.table("datos/airfoil_self_noise.txt",header=T, na.strings="?")
Airfoil=na.omit(Airfoil) #Elimina las muestras que tienen algún dato perdido
attach(Airfoil)

#Librerias utilizadas
library(ISLR)
library(e1071)
library(MASS)
library(car)
library(leaps)
library(DAAG)
library(boot)

#Analisis de la base de datos

summary(Airfoil) #Muestra información básica sobre la base de datos

cor(Airfoil) #Muestra la matriz de correlación entre las variables de la BD

pairs(Airfoil) #Muestra todas las gráficas de todas las variables frente a todas las variables de la base de datos

hist(SSPL, freq = FALSE, main = "Histograma de SSPL") #Histograma de la variable respuesta (SSPL)
curve(dnorm(x, mean(SSPL), sd(SSPL)), col = 1, lty = 1, lwd = 2, add=T) #Dibuja la campana de Gauss

boxplot(SSPL, horizontal = TRUE, main = "Diagrama de cajas para SSPL") #Diagrama de cajas de SSPL

#Cáculo de varianzas
SSPL.menor = SSPL [SSPL < median(SSPL)]
SSPL.mayor = SSPL [SSPL > median(SSPL)]

var(SSPL.menor)
var(SSPL.mayor)

var(SSPL.menor)/var(SSPL.mayor)

#Selección de variables
set.seed(1)
indices = sample (dim(Airfoil)[1], 1200, replace=FALSE)
train = Airfoil[indices, ]
test = Airfoil[-indices, ]

SSPL.train = train[, "SSPL"]
SSPL.test = test[, "SSPL"]

regfit.full = regsubsets(SSPL~Frequency*AOA*CL*FSV*SSDT, data = train, nvmax = 31, method = "exhaustive")
reg.summary = summary(regfit.full)
reg.summary
plot(reg.summary$rsq, xlab = " Número de variables ", ylab = "RSQ", type ="l", main = "Selección de variables")

    #Modelo a ajustar
modelo = SSPL ~ Frequency + AOA + FSV + Frequency:AOA + Frequency:CL + AOA:CL + Frequency:FSV + AOA:FSV + CL:FSV + Frequency:SSDT + CL:SSDT + FSV:SSDT + Frequency:AOA:CL + Frequency:CL:FSV + Frequency:AOA:SSDT + Frequency:CL:SSDT + AOA:FSV:SSDT + Frequency:AOA:CL:FSV + Frequency:AOA:CL:SSDT + Frequency:CL:FSV:SSDT

#Ajuste de modelos
set.seed(1)

#Regresión generalizada múltiple
lmFit = lm(modelo, data = train)
summary(lmFit)
pred = predict.lm(lmFit, test, type = "response", se.fit = TRUE)

names(pred)
mean(pred$se.fit)
library(Metrics)
mse(SSPL.test,pred$fit)
termplot(lmFit)

#SVM
set.seed (1)
  #Buscamos el valor çoptimo para cost y gamma
svm.fit = svm(modelo, data = train, kernel = "radial")
pred= predict(svm.fit, test)
plot(svm.fit, data=test)


library(caret)
ctrl <- trainControl(method = "cv", savePred=T)
mod <- train(modelo, data=train, method = "svmLinear", trControl = ctrl)
head(mod$pred)



################Eliminar todas las variables################################
rm(list=ls()) 
