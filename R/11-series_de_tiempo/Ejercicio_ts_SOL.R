################################################################
#################         DATOS           ######################
################################################################

# Este conjunto de datos contiene el n?mero mensual de pasajeros de una aerol?nea, desde enero de 1949 hasta diciembre de 1960.

data(AirPassengers)
datos <- AirPassengers


# Exploracion de los datos

plot(datos)

# Gráficos estacionales
ggseasonplot(datos, year.labels = TRUE, year.labels.left = TRUE)+
  ylab("Ventas")+
  ggtitle("Gráfico estacional: pasajeros aerolinea")+
  theme_minimal()



# Division de datos en train y test


datos.train <- window(datos, start=c(1949, 1), end=c(1958, 12))
datos.test <- window(datos, start=c(1959, 1), end=c(1960, 12))

# Ajuste de 2 modelos ingenuos

#ingenuo estacional
modelo_ing_est <- snaive(datos.train, h = 24)
summary(modelo_ing_est)

#ingenuo promedio
modelo_prom <- meanf(datos.train, h = 24)
summary(modelo_prom)

# Ajuste de modelo ETS

modelo1 <- ets(datos.train, model = "ZZZ", opt.crit = "mse", ic = "aic")
summary(modelo1)
plot(modelo1)


# Ajuste de modelo ARIMA

modelo2 <- auto.arima(datos.train)
summary(modelo2)


# Validaci?n modelos

adf.test(residuals(modelo1))
Box.test(residuals(modelo1), lag=20, type="Ljung-Box")

adf.test(residuals(modelo2))
Box.test(residuals(modelo2), lag=20, type="Ljung-Box")

plot(forecast(modelo1,h=24,level=95))
plot(forecast(modelo2,h=24,level=95))

# Con ETS
pron_ets <- forecast(object = modelo1, h = 24)
pron_ets

# Con ARIMA
pron_arima <- forecast(modelo2, h = 24)
pron_arima

accuracy(modelo_ing_est, datos.test)
accuracy(modelo_prom, datos.test)
accuracy(pron_ets, datos.test)
accuracy(pron_arima, datos.test)

# Grafica de modelo ARIMA
 

plot(forecast(modelo1,h=36,level=95))
plot(forecast(modelo2,h=36,level=95))


#El mejor fue el ETS,se entrena con todo y se pronostica 1961

modelo1 <- ets(datos, model = "ZZZ", opt.crit = "mse", ic = "aic")
summary(modelo1)
plot(modelo1)

plot(forecast(modelo1,h=12,level=95))
