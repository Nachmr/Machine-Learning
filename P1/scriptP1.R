path = "~/Copy/Informatica/Carrera/3º/2º Cuatrimestre/AA/Prácticas/P1/datos"

#######################################
#Ejercicio 1
#######################################
library(ISLR)
library(MASS)
fix(Boston)
attach(Boston)

#2.
plot(black,crim) #criminialidad vs negro
plot(age,tax) #edad vs valor de vivivienda/10.000
plot(nox,indus) #oxigeno e industria

#3.
#miramos el p valor y el coeficiente de correlacion
summary(lm(crim~Boston[[2]]))
summary(lm(crim~Boston[[3]]))
summary(lm(crim~Boston[[4]]))
summary(lm(crim~Boston[[5]]))
summary(lm(crim~Boston[[6]]))
summary(lm(crim~Boston[[7]]))
summary(lm(crim~Boston[[8]]))
summary(lm(crim~Boston[[9]]))
summary(lm(crim~Boston[[10]]))
summary(lm(crim~Boston[[11]]))
summary(lm(crim~Boston[[12]]))
summary(lm(crim~Boston[[13]]))
summary(lm(crim~Boston[[14]]))

cor(Boston)

plot(ptratio,crim)

#4
#a)
plot(crim)
#c)
plot(tax)
#b)
plot(ptratio)

#5

n <-0
for(j in chas){
  if(j==1){
    n<-n+1
  }
}
n
#6
mean(ptratio)

#7
min(medv)
plot(medv)
Boston[399,]

#8
#a) igual que el 5 pero con rm y cambiando el if

n <-0
for(j in rm){
  if(j>7){
    n<-n+1
  }
}

n

n <-0
for(j in rm){
  if(j>8){
    n<-n+1
  }
}

n

#######################################
#Ejercicio 2
#######################################

#apartado a)

# i)
lm(crim~Boston[[2]]) #crim vs zn
lm(crim~Boston[[3]]) #crim vs indus
lm(crim~Boston[[4]]) #crim vs chas
lm(crim~Boston[[5]]) #crim vs nos
lm(crim~Boston[[6]]) #crim vs rm
lm(crim~Boston[[7]]) #crim vs age
lm(crim~Boston[[8]]) #crim vs dis
lm(crim~Boston[[9]]) #crim vs rad
lm(crim~Boston[[10]]) #crim vs tax
lm(crim~Boston[[11]]) #crim vs pratio
lm(crim~Boston[[12]]) #crim vs black
lm(crim~Boston[[13]]) #crim vs lstat
lm(crim~Boston[[14]]) #crim vs medv

#ii)
summary(lm(crim~Boston[[6]])) 
sd(Boston[[6]])


#iii)
plot(lm(crim~Boston[[6]]))
plot(lm(crim~Boston[[9]]))


#b)
#i)

lm.fit = lm(crim~. , data = Boston)
plot(lm.fit)

#ii)
summary(lm.fit)

#c)

###Calculamos los valores del eje X
for(name in names(Boston)){ #vamos recorrieno cada columna
 if(name == "zn"){ #Inicializacion de resimple y x
   resimple <- lm(crim~Boston[,name]) #calculamos la recta de regresion
   ejex <- resimple$coefficient #guardamos el valor del coeficiente de la recta en x
 } 
 
 if(name != "crim" && name != "zn"){ #si no es la variable crim o zn (zn la hemos incluido en la inicialización)
   resim <- lm(crim~Boston[,name])
   ejex <- union(ejex,resim$coefficients[2])
 }
  
}

###Calculamos los valores del eje Y
remul = lm(crim~. , data=Boston)
ejey <- remul$coefficients

plot(ejex,ejey)

#d)

summary(lm(crim~Boston[[2]]+I(Boston[[2]]^2)+I(Boston[[2]]^3)))
summary(lm(crim~Boston[[3]]+I(Boston[[3]]^2)+I(Boston[[3]]^3)))
summary(lm(crim~Boston[[4]]+I(Boston[[4]]^2)+I(Boston[[4]]^3)))
summary(lm(crim~Boston[[5]]+I(Boston[[5]]^2)+I(Boston[[5]]^3)))
summary(lm(crim~Boston[[6]]+I(Boston[[6]]^2)+I(Boston[[6]]^3)))
summary(lm(crim~Boston[[7]]+I(Boston[[7]]^2)+I(Boston[[7]]^3)))
summary(lm(crim~Boston[[8]]+I(Boston[[8]]^2)+I(Boston[[8]]^3)))
summary(lm(crim~Boston[[9]]+I(Boston[[9]]^2)+I(Boston[[9]]^3)))
summary(lm(crim~Boston[[10]]+I(Boston[[10]]^2)+I(Boston[[10]]^3)))
summary(lm(crim~Boston[[11]]+I(Boston[[11]]^2)+I(Boston[[11]]^3)))
summary(lm(crim~Boston[[12]]+I(Boston[[12]]^2)+I(Boston[[12]]^3)))
summary(lm(crim~Boston[[13]]+I(Boston[[13]]^2)+I(Boston[[13]]^3)))
summary(lm(crim~Boston[[14]]+I(Boston[[14]]^2)+I(Boston[[14]]^3)))


#######################################
#Ejercicio 3
#######################################
#cambiar al directorio donde se encuentra el archivo Auto.csv
setwd(path)
Auto = read.csv("Auto.csv", header =T, na.strings ="?")
Auto = na.omit(Auto)
attach(Auto)

#1
pairs(Auto) #scatteplot

#2
Autocuantitativa = c("mpg","displacement","horsepower","weight","acceleration","year") #subtabla con las variables cuantitativas
head(Auto[Autocuantitativa])
cor(Auto[Autocuantitativa])

#3
regmul <- lm(mpg~year+horsepower+weight+acceleration+displacement)
summary(regmul) 
#4
confint(regmul, level = 0.95) #por defecto al 95%

#5
plot(regmul)

#6
summary(lm(mpg~weight*horsepower+acceleration:horsepower))

