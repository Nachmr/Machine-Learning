path = "~/Copy/Informatica/Carrera/3º/2º Cuatrimestre/AA/Prácticas/P2"

#######################################
#Ejercicio 1
#######################################
library(ISLR)
library(MASS)
fix(Weekly)
attach(Weekly)

# Apartado 1: #med, desv tipica, percentiles...
plot(Weekly)
summary(Weekly)
cor(Weekly[,-9])
Year[which.min(Today)]

# Apartado 2
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Weekly, family=binomial)              
summary(glm.fit)
#nos fijamos en el p-valor y si el estimado es positivo o negativo. En este caso se podria decir que lag2 tiene alguna relacion, dado que su p-valor es bajo. Aunque no tanto como para asegurarlo con certeza. pag 157 ISLR

#Apartado 3
#la suma de su diagonal nos dice cuanto he acertado del total. Nos dice lo que nos confundimos entre una clase y otra clase
dim(Weekly)
contrasts(Direction)
glm.probs = predict( glm.fit , type ="response")
glm.pred = rep("Down",1089)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)
(54+557)/1089 #Calculo del porcentaje de acierto
mean(glm.pred==Direction)

#Apartado 4

#Para año MENOR que 2008
reg2008 = (Year < 2009)
Weekly.2008= Weekly[reg2008,]
dim(Weekly.2008)
Direction.2008 = Direction[reg2008]
glm.fit = glm(Direction.2008~Lag2, data = Weekly.2008, family=binomial)              
glm.probs = predict( glm.fit , type ="response")
glm.pred = rep("Down",985)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2008)
mean(glm.pred==Direction.2008)

#Para años entre 2009 y 2010
Weekly.200910 = Weekly[!reg2008,]
dim(Weekly.200910)
Direction.200910 = Direction[!reg2008]
glm.fit = glm(Direction.200910~Lag2, data = Weekly.200910, family=binomial)              
glm.probs = predict( glm.fit , type ="response")
glm.pred = rep("Down",104)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.200910)
(8+57)/104

#Apartado 5
#LDA:
lda.fit = lda(Direction.200910~Lag1, data = Weekly.200910)
lda.pred = predict(lda.fit,Weekly.200910)
lda.class = lda.pred$class
table(lda.class,Direction.200910)
mean(lda.class == Direction.200910)

#QDA
qda.fit = qda ( Direction.200910~Lag1, data =  Weekly.200910)
qda.class = predict(qda.fit,Weekly.200910)$class
table(qda.class,Direction.200910)
mean(qda.class == Direction.200910)

#KNN (pag 164 ISLR)
library(class)
train.X = cbind(Lag1)[reg2008,]
test.X = cbind(Lag1)[!reg2008,]
train.Direction = Direction[reg2008]

set.seed(1)
knn.pred = knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2008)
mean(knn.pred == Direction.2008)





