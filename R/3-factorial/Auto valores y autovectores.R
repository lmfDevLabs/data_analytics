#Leer los datos
cartera<-read.csv("cartera.csv", header=TRUE, sep=";", dec=",")

str(cartera)

#Eliminar las variables cualitativas
cartera1 <- cartera[,-c(2,9)]
str(cartera1)

#Matriz de covarianza
CovMatriz <- cor(cartera1)
round(CovMatriz,4)

#Mapa de calor de correlaciones
heatmap(CovMatriz, cexCol = 0.7, cexRow = 0.7)

#Valores y vectores propios de la matriz de covarianzas
auto <- eigen(CovMatriz)
auto

valores <- auto$values
valores
valores[1] #La primer componente captura la variabilidad equivalente a 3.2 variables

#Raíz cuadrada de los valores propios
#corresponden a las desviaciones estándar capturadas por la respectiva componente
sqrt(auto$values)

#Suma de valores propios (total de variables)
total <- sum(auto$values)
total

#Proporción de variabilidad capturada por la primer componente

prop1 <- valores[1]/sum(valores)
prop1

#Creando el vector de variabilidad capturada
proporcion <- vector()

for (i in 1:7){
  proporcion[i]=valores[i]/total
}

proporcion

#Variabilidad acumulada
cumsum(proporcion)

#Dirección de las componentes
vectores <- auto$vectors
vectores

#Son vectores unitarios
norm_vec <- function(x) sqrt(sum(x^2))
norm_vec(vectores[,1])
