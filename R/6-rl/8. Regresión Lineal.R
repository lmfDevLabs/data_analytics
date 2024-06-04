if (!require('corrplot')) install.packages('corrplot')
if (!require('lmtest')) install.packages('lmtest')
if (!require('MASS')) install.packages('MASS')
if (!require('leaps')) install.packages('leaps')
if (!require('RColorBrewer')) install.packages('RColorBrewer')
if (!require('Metrics')) install.packages('Metrics')
if (!require('forecast')) install.packages('forecast')
if (!require('tidyverse')) install.packages('tidyverse')


## Revisar que el archivo cartera se encuentre en documentos, 
## o la ruta en donde se encuentra direccionado R

getwd()


#leo el archivo
regtecnica <- read.csv("regtecnica.csv", header=TRUE, 
                       sep=";", dec=",")
regtecnica %>% head()

regtecnica %>% glimpse()
regtecnica %>% dim()

#obtengo una muestra
set.seed(6328) #se deja alguna semilla para que el muestreo sea replicable
#aquí se define el tamaño de la muestra, en este caso entrenamiento tendrá el 80% de los casos
sample <- sample.int(nrow(regtecnica), floor(.8*nrow(regtecnica)))
regtecnica.train <- regtecnica[sample, ]
regtecnica.test <- regtecnica[-sample, ]


# Enfoque estadística clásica ----------------------------------------

matrizcor <- cor(regtecnica.train)
corrplot(matrizcor, method="square", tl.cex = 0.7,col=brewer.pal(n=8, name="PuOr"),addCoef.col = "black", 
         number.cex=0.7,type = "upper", diag = FALSE)


#CREANDO MODELOS BÁSICOS

#un modelo con variables elegidas (preespecificado)
modelo0 <- lm(potenciaresp ~ potencianodo + picovalle,
              data=regtecnica.train)

# un modelo con todas las variables (preespecificado)
modelo1 <- lm(potenciaresp ~.,data=regtecnica.train)

#pruebas de hipótesis y dos métricas
summary(modelo0)
summary(modelo1)

#Intervalos de confianza
confint(modelo0, level=0.95)


layout(matrix(c(1,2,3,4),2,2)) # opcional 4 gráficos/página
plot(modelo0)

#library(lmtest)
dwtest(modelo0)
dwtest(modelo1)

#Supuestos modelo completo

layout(matrix(c(1,2,3,4),2,2)) # opcional 4 gráficos/página
plot(modelo1)


# Enfoque Machine Learning ------------------------------------------------

#obteniendo el AIC
AIC(modelo0)
AIC(modelo1)

#PREDICCIONES
#hacer predicciones
pred0 <- predict(modelo0, regtecnica.test, se.fit = TRUE)
pred0$fit
pred1 <- predict(modelo1, regtecnica.test, se.fit=TRUE)


RMSE0 <- sqrt(mean((pred0$fit-regtecnica.test$potenciaresp)^2))
RMSE1 <- sqrt(mean((pred1$fit-regtecnica.test$potenciaresp)^2))
RMSE0
RMSE1

#RMSE por medio del paquete Metrics
rmse(regtecnica.test$potenciaresp,pred0$fit)
rmse(regtecnica.test$potenciaresp,pred1$fit)

#RMSE y otras métricas de ajuste con el paquete forecast
accuracy(pred0$fit, regtecnica.test$potenciaresp)
accuracy(pred1$fit, regtecnica.test$potenciaresp)


#Feature selection
##stepwise
modelostep <- step(modelo1,direction="both")
summary(modelostep)
plot(modelostep)
dwtest(modelostep)

##best subsets
# library(MASS)
# library(leaps)
#nbest (número de modelos por cada k) es por defecto 1 y nvmax es por defecto 8 (k máximo)
modelsub <- regsubsets(potenciaresp~.,data=regtecnica.train, 
                       nbest=1, nvmax=6, method = "exhaustive") 
summary(modelsub)


#obtener r2 ajustado
summary(modelsub)$adjr2
#obtener el cp
summary(modelsub)$cp
#obtener el BIC (medida similar al AIC)
summary(modelsub)$bic


#Desempeño en la base de validación
predstep <- predict(modelostep, regtecnica.test, se.fit=TRUE)
# predsub <- predict(modelsub, regtecnica.test, se.fit=TRUE)

RMSEstep <- sqrt(mean((predstep$fit-regtecnica.test$potenciaresp)^2))
RMSE0
RMSE1
RMSEstep

#con la librería Metrics
rmse(regtecnica.test$potenciaresp, predstep$fit)
# rmse(regtecnica.test$potenciaresp, predsub$fit)


##feature engineering
#extraer atipicos
atipicos <- c("3","25","78")
veratipicos <- regtecnica.train[atipicos,]
#obtener su media en todas las variables
atiptc <- apply(veratipicos,2,mean)
#obtener la media general y comparar
todostc <- apply(regtecnica.train,2,mean)
comparar <- as.data.frame(cbind(atiptc,todostc))
comparar

dev.off()
plot(regtecnica.train$distancianodo,modelostep$residuals)

#Gráfico de distancia vs potencia de respuesta
regtecnica.train %>% ggplot(aes(x=distancianodo, y=potenciaresp)) +
  geom_point()

#entrenamiento
regtecnica.train <-  regtecnica.train %>% 
  mutate(inverso = 1/distancianodo,
         inverso2 = 1/(distancianodo^2))

#validacion
regtecnica.test <-  regtecnica.test %>% 
  mutate(inverso = 1/distancianodo,
         inverso2 = 1/(distancianodo^2))

modelo2 <- lm(potenciaresp ~.,data=regtecnica.train)
modelo2step <- step(modelo2)
summary(modelo2step)

layout(matrix(c(1,2,3,4),2,2))
plot(modelo2step)

dwtest(modelo2step)

## Predicciones
predstep2 <- predict(modelo2step, regtecnica.test, se.fit=TRUE)

RMSEstep2 <- sqrt(mean((predstep2$fit-regtecnica.test$potenciaresp)^2))
RMSE0
RMSE1
RMSEstep
RMSEstep2

