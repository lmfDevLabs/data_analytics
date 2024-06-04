if (!require('forecast')) install.packages('forecast')
if (!require('tseries')) install.packages('tseries')

if (!require('tsoutliers')) install.packages('tsoutliers')
if (!require('tidyverse')) install.packages('tidyverse')

## Lectura del archivo  
## Revisar que el archivo se encuentre en documentos, 
## o la ruta en donde se encuentra direccionado R

getwd()

festivos <- read.csv("festivos.csv", header=TRUE, sep=";", dec=",")

# Revisión de la estructura y ver los primeros 3 registros del conjunto de datos
festivos %>% glimpse()
head(festivos,3)

# Construir para cada serie un objeto tipo serie de tiempo. 
producto1 <- ts(festivos$FIESTA1, start=c(2007, 1), end=c(2014, 2), frequency=12)
producto2 <- ts(festivos$FIESTA2, start=c(2007, 1), end=c(2014, 2), frequency=12)
# por supuesto puedo tenerlos todos a una
productos <- ts(festivos[1:4],start=c(2007, 1), end=c(2014, 2), frequency=12)


# realizar una muestra de entrenamiento (Enero 2007 a Diciembre 2012)
producto1.train <- window(producto1, start=c(2007, 1), end=c(2012, 12))
producto2.train <- window(producto2, start=c(2007, 1), end=c(2012, 12))
# realizar una muestra de validación (Enero 2013 a Febrero 2014)
producto1.test <- window(producto1, start=c(2013, 1), end=c(2014, 2))
producto2.test <- window(producto2, start=c(2013, 1), end=c(2014, 2))


# Exploración gráfica
plot(producto1.train) #Paquete básico
autoplot(producto1.train) #Paquete forecast

# Gráficos estacionales
ggseasonplot(producto1.train, year.labels = TRUE, year.labels.left = TRUE)+
  ylab("Ventas")+
  ggtitle("Gráfico estacional: ventas producto 1")+
  theme_minimal()

ggseasonplot(producto1.train, polar = TRUE)+
  ylab("Ventas")+
  ggtitle("Gráfico estacional: ventas producto 1")


# Términos diferenciados
# Lag 1
Serie_dif1 <- diff(producto1.train, differences = 1)
head(Serie_dif1)
head(producto1.train)
# Lag 2
Serie_dif2 <- diff(producto1.train, lag = 2, differences = 1)
head(Serie_dif2)

# Diferenciación iterada
Serie_dif_it_2 <- diff(producto1.train, differences = 2)
head(Serie_dif_it_2)

# Gráfico de la serie diferenciada
autoplot(Serie_dif1)

# Diferenciación estacional
Serie_dif_est_1 <- diff(producto1.train, lag = 12, differences = 1)
autoplot(Serie_dif_est_1)
ggseasonplot(Serie_dif_est_1)

# Correlogramas
Acf(producto1.train, main = "Correlograma")
Pacf(producto1.train, main = "Correlograma parcial")
#Umbrales
+2/sqrt(length(producto1.train))
-2/sqrt(length(producto1.train))

# eliminando el efecto estacional
Acf(Serie_dif_est_1, main = "Correlograma serie diferenciada")
Pacf(Serie_dif_est_1, main = "Correlograma parcial serie diferenciada")

# creo desplazamientos
ar1 <- stats::lag(producto1.train, -1)
ar2 <- stats::lag(producto1.train, -2)
ar3 <- stats::lag(producto1.train, -3)
ar4 <- stats::lag(producto1.train, -4)
#los uno
base <- as.data.frame(cbind(producto1.train,ar1,ar2,ar3,ar4))
base
#dejo solo los datos completos
autocormatrix <- base[5:72,]
#obtengo la matriz de autocorrelaciones para k=5
cor(autocormatrix[,1:5])
#comparo con el gráfico
Acf(producto1.train, main = "Correlograma")

#una forma de entender las autocorrelaciones parciales
parciales1 <- lm(producto1.train~ar1, data = autocormatrix)
coefficients(parciales1)
parciales <- lm(producto1.train~ar1+ar2, data = autocormatrix)
coefficients(parciales)
parciales2 <- lm(producto1.train~ar1+ar2+ar3, data = autocormatrix)
coefficients(parciales2)
parciales3 <- lm(producto1.train~., data = autocormatrix)
coefficients(parciales3)


#Modelos ingenuos

#ingenuo
modelo_ing <- naive(producto1.train, h = 14)
summary(modelo_ing)

#ingenuo estacional
modelo_ing_est <- snaive(producto1.train, h = 14)
summary(modelo_ing_est)

#ingenuo promedio
modelo_prom <- meanf(producto1.train, h = 14)
summary(modelo_prom)

#ingenuo drift
modelo_drift <- rwf(producto1.train, drift = TRUE, h = 14)
summary(modelo_drift)

#modelo
modelo1 <- ets(producto1.train, model = "ZZZ", opt.crit = "mse", ic = "aic")
summary(modelo1)

fitted(modelo1)

#auto.arima busca el mejor arima. 
modelo2 <- auto.arima(producto1.train)
summary(modelo2)

#auto.arima con regresora
modelo3 <- auto.arima(producto1.train,xreg=producto2.train)
summary(modelo3)

#Verificación de supuestos

autoplot(residuals(modelo1), main = "Residuales modelo ETS")
autoplot(residuals(modelo2), main = "Residuales modelo ARIMA")

adf.test(residuals(modelo1))
Box.test(residuals(modelo1), lag=20, type="Ljung-Box")

adf.test(residuals(modelo2))
Box.test(residuals(modelo2), lag=20, type="Ljung-Box")

auto.arima(residuals(modelo1))

auto.arima(residuals(modelo2))

#Gráfico del modelo ets(descomposición)
plot(modelo1)

#Pronósticos
#Gráficos de pronóstico
plot(forecast(modelo1,h=14,level=95))
plot(forecast(modelo2,h=14,level=95))

#Predicción en validación
modelo.test <- ets(producto1.test, model=modelo1,
                   use.initial.values = TRUE)
modeloar.test <- Arima(producto1.test, model=modelo2, 
                       use.initial.values=TRUE)

accuracy(modelo.test)
accuracy(modeloar.test)

modeloar.test$fitted

# Pronósticos ----
# Con ETS
pron_ets <- forecast(object = modelo1, h = 14)
pron_ets

# Con ARIMA
pron_arima <- forecast(modelo2, h = 14)
pron_arima

# Con ARIMAX
producto2.test <- window(producto2, start = c(2013, 1), end = c(2014, 2))

pron_arimax <- forecast(object = modelo3, h = 14, xreg = producto2.test)
pron_arimax

# Métricas de pronóstico
accuracy(pron_ets, producto1.test)
accuracy(pron_arima, producto1.test)
accuracy(pron_arimax, producto1.test)
# ingenuo
accuracy(modelo_ing, producto1.test)
# ingenuo estacional
accuracy(modelo_ing_est, producto1.test)
# ingenuo promedio
accuracy(modelo_prom, producto1.test)
# ingenuo drift
accuracy(modelo_drift, producto1.test)


# Validación escalonada ----
# Validación escalonada ARIMA
far2 <- function(x,h){
  forecast(Arima(producto1, order = c(0,0,0), seasonal = c(2,1,0)), 
           h=h)
  }
errores <- tsCV(producto1, far2, h = 12, initial = 24)

erroresd <- as.data.frame(errores)[25:86,]
erroresd2 <- (erroresd)^2

RMSEs <- apply(erroresd2, 2, function(y) sqrt(mean(y, na.rm = TRUE)))
RMSEs

tibble(h = 1:12, RMSE = RMSEs) %>% 
  ggplot(aes(x = h, y = RMSE)) +
  geom_point()

mean(RMSEs, na.rm = TRUE)

# Validación escalonada ETS
far3 <- function(x,h){
  forecast(ets(producto1.train, model = "MNM", opt.crit = "mse", ic = "aic"), 
           h=h)
  }
errores2 <- tsCV(producto1, far3, h = 12, initial = 24)

erroresda <- as.data.frame(errores2)[25:86,]
erroresd2a <- (erroresda)^2

RMSEsa <- apply(erroresd2a,2,function(y) sqrt(mean(y,na.rm = TRUE)))
RMSEsa

mean(RMSEsa, na.rm = TRUE)



#Análisis de outliers
# https://datascienceplus.com/outliers-detection-and-intervention-analysis/
result <- tso(producto1.train)
result
#estos son los outliers
result$outliers
#representación gráfica del efecto de los outliers
plot(result)
# Veamos la regresora (con efecto decreciente)
result$fit$xreg
#Muestra los coeficientes, incluido los regresores de los outliers para quitarlos
result$fit$coef

# Los parámetros del SARIMA
result$fit$arma

result$fit$fitted


