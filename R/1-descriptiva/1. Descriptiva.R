## Lectura del archivo cartera 
## Revisar que el archivo cartera se encuentre en documentos, 
## o la ruta en donde se encuentra direccionado R

getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Copiar los archivos descargados en la ruta que surge de correr el 
## comando anterior

cartera <- read.csv("cartera.csv", header=TRUE, sep=";", dec=",")

## Instalación de paquetes
# install.packages("readxl")
# paquetes <- c("psych","e1071", "ggplot2")
# install.packages(paquetes)

## Carga de paquetes
library(readxl)
library(psych)
library(e1071)
library(ggplot2)

## Lectura del archivo en excel
telco <- read_excel("services2.xls")

## Visualización del inicio y el final del conjunto de datos
head(telco, n=3)
tail(telco)

## Dimensión del conjunto
dim(telco)

## Nombres de las columnas
colnames(telco)

## Revisar la estructura del objeto que se tiene y los diferentes 
## tipos de datos mediante el comando str
str(cartera)

## Revisar el tipo de dato de una variable en particular
## mediante el comando class

class(cartera$yearsadress)
class(cartera$retrasos)

## Cambio en el tipo de dato de las variables "yearadress", "retrasos" y "tipoips"
## mediante los comandos as.numeric() y as.factor()
cartera$yearsadress <- as.numeric(cartera$yearsadress)
cartera$retrasos <- as.factor(cartera$retrasos)
cartera$tipoips <- as.factor(cartera$tipoips)

#Veamos cómo quedó el conjunto
str(cartera)

# --- Paquete dplyr
# install.packages("dplyr")
library(dplyr)

# Uso de la función glimpse
telco %>% glimpse()
cartera %>% glimpse()

# --- 

#######

## Resumen general de las variables del objeto telco
summary(telco)
summary(cartera)

## Gráfico de la variable bill
plot(telco$bill)
plot(telco$bill, col="blue", type="p", xlab="cliente", ylab="pago factura", cex=.5, main="Gráfico")

plot(telco$score)

#Gráfico usando ggplot
#Básico
ggplot(telco, aes(x=seq_along(bill), y=bill))+
  geom_point()

#Con formato
ggplot(telco, aes(x=seq_along(bill), y=bill))+
  ggtitle("Gráfico")+
  labs(x="cliente", y="pago factura")+
  geom_point(size=1, col="red")+
  theme_minimal()


## Hacer que R entienda el nombre de las variables de un data frame
## como si fueran objetos separados
attach(telco)

####### Medidas de Tendencia Central

## Cálculo del promedio de la variable bill que pertenece al data frame
## telco
mean(bill)

## Cálculo de la mediana de la variable bill que pertenece al data frame
## telco
median(bill)

## Cálculo del promedio recortado de la variable bill que pertenece 
## al data frame telco  
mean(bill, trim=0.2)

####### Medidas de dispersión

## Cálculo del rango 
range(bill)
## Cálculo de la varianza 
var(bill)
## Cálculo de la desviación estándar 
sd(bill)
## Cálculo del rango intercuartílico
IQR(bill)
## Cálculo del MAD
mad(bill)


## Coeficiente de variación específico para la variable bill
cv=(100*sd(bill))/mean(bill)
cv

## Función del coeficiente de variación
CV <- function(var){(sd(var)/mean(var))*100}
CV(bill)
CV(minutes)

## Realiza el cálculo del CV para cada columna del data frame telco
apply(telco,2,CV)

### Carga del paquete psych
## Uso de la función describe que calcula múltiples descriptivos
describe(telco)

## Revisión del objeto que crea la función describe
telcostat <- describe(telco)
str(telcostat)

### Adición del CV a los descriptivos previos
cvs <- apply(telco,2,CV)
telcostat2 <- cbind(telcostat,cvs)

## Cálculo de la media geométrica de los CV
geometric.mean(telcostat2$cvs)


########

### Revisión de los cuantiles de la variable bill
quantile(bill)
quantile(bill, c(.10,.45,.90))

## Cálculo de los cuantiles para las columnas del data frame telco
cuantiles<-apply(telco,2,quantile)
cuantiles

## Descriptivos agrupados por una variable factor
carterastat <- describeBy(cartera, cartera$retrasos)
carterastat


####### Gráficos univariados

### Histograma
hist(bill)
hist(bill, breaks=15, col="red", border = FALSE, main="Histograma de facturación", 
     xlab="Facturación", ylab="Frecuencia")

hist(minutes)
hist(score)

## Usando ggplot
# Básico
ggplot(telco, aes(x=bill))+
  geom_histogram()

# Con formato 
ggplot(telco, aes(x=bill))+
  geom_histogram(bins = sqrt(length(bill)), color="black")+
  ggtitle("Histograma de facturación")+
  labs(x="Facturación", y="Densidad")+
  theme_minimal()

#Agregando la curva de densidad de los datos

ggplot(telco, aes(x=bill, y=..density..))+
  geom_histogram(bins = sqrt(length(bill)), color="black")+
  geom_density(aes(x=bill, y=..density..), color="red", size=1)+
  ggtitle("Histograma de facturación con densidad")+
  labs(x="Facturación", y="Densidad")+
  theme_minimal()

## Comparando con la distribución normal

ggplot(telco, aes(x=bill, y=..density..))+
  geom_histogram(bins = sqrt(length(bill)), color="black")+
  geom_line(aes(x=bill, y=dnorm(bill, mean(bill), sd(bill))), color="red", size=1)+
  ggtitle("Histograma de facturación con curva normal")+
  labs(x="Facturación", y="Densidad")+
  theme_minimal()

### Boxplot
boxplot(bill)
boxplot(bill, horizontal = TRUE, col="green")

boxplot(minutes)

## Usando ggplot

# Básico
ggplot(telco, aes(bill))+
  geom_boxplot()

# Con formato

#TAREA: pintar el boxplot con formato en ggplot.


### QQ norm
qqnorm(minutes)
qqnorm(bill)

#TAREA: pintar el qqplot usando ggplot.

## Revisión de la estructura de los datos de gráficos
boxguarda<-boxplot(bill)
str(boxguarda)

## Extracción de los datos atípicos
boxguarda$out
length(boxguarda$out)

### Descripción de las variables categóricas
table(cartera$tipoips)

### Gráfica de la tabla de contingencia
tabla1 <- table(cartera$tipoips)
barplot(tabla1, main="Cantidad de IPS", xlab="Tipo de IPS")
barplot(tabla1, main="Cantidad de IPS", ylab="Tipo de IPS", xlab="Frecuencia", col="brown", horiz=TRUE)

## Gráfico con ggplot

ggplot(cartera, aes(tipoips))+ 
  geom_bar()+
  ggtitle("Diagrama de barras tipo de IPS")+
  labs(x="Tipo de IPS", y="")+
  theme_minimal()


### Cálculo de proporción de las frecuencias de la tabla 
prop.table(tabla1)
round(prop.table(tabla1),4)


####### Cruces básicos de variables

plot(minutes, bill)
plot(minutes, bill, col="purple", cex=.5)

## Diagrama de dispersión con ggplot

ggplot(telco, aes(x=minutes, y=bill))+
  geom_point(size=1)+
  ggtitle("Diagrama de dispersión minutos vs facturación")+
  labs(x="minutos", y="facturación")+
  theme_minimal()


### boxplot dividido por una variable categórica
boxplot(pasivo_sobre_venta~retrasos, data=cartera)


## Con ggplot
ggplot(cartera, aes(x=retrasos, y=MESES))+
  geom_boxplot()+
  ggtitle("Boxplot de meses segmentado por retrasos")+
  labs(x="Retrasos", y="Meses")+
  theme_minimal()

# Boxplots agrupados
ggplot(cartera, aes(x=tipoips, y=MESES, fill=retrasos))+
  geom_boxplot()+
  ggtitle("Boxplot de meses segmentado por retrasos")+
  labs(x="Tipo IPS", y="Meses")+
  theme_minimal()

###### Cruce de variables categóricas
table(cartera$tipoips, cartera$retrasos)
table2 <- table(cartera$tipoips, cartera$retrasos)
prop.table(table2) #Proporción con respecto al total de datos cruzados
prop.table(table2, margin = 1) #Proporción por filas
prop.table(table2, margin = 2) #Proporción por columnas

## Diagramas de barras para tablas de contingencia
# IPS Con retraso
ggplot(cartera, aes(tipoips, fill=retrasos))+ 
  geom_bar()+
  ggtitle("Diagrama de barras tipo de IPS")+
  labs(x="Tipo de IPS", y="")+
  theme_minimal()

# Comparativo
ggplot(cartera, aes(tipoips, fill=retrasos))+ 
  geom_bar(position = "fill")+
  ggtitle("Diagrama de barras tipo de IPS con retrasos")+
  labs(x="Tipo de IPS", y="")+
  theme_minimal()

## Dejar de usar las variables del data frame telco
detach(telco)

## Usar los nombres de las variables del data frame cartera como objetos
attach(cartera)

## Carga de la librería gmodels
install.packages("gmodels")
library("gmodels")

### Una mejor creación de tablas cruzadas
CrossTable(tipoips, retrasos)

######
install.packages("vcd")
library("vcd")

## Gráfico de mosaico para tablas cruzadas
mosaic(~tipoips + retrasos,data=cartera, 
       legend=TRUE, shade=TRUE)


#Ejemplo adicional de mosaico
library(MASS)
data("Titanic")
mosaic(Titanic,shade=TRUE)

## Formula interface for tabulated data plus shading and legend:
mosaic(~ Sex + Age + Survived, data = Titanic,
       main = "Survival on the Titanic", shade = TRUE, legend = TRUE)
