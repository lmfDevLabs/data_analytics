# CARGAR PAQUETES

library(readxl)
library(caret)
library(ROCR)
library(lift)
library(tidyverse)
library(MLmetrics)


# CARGAR DATOS

datos <- read_excel("Datos_HR.xlsx")
datos %>% glimpse()

# PROCESAMIENTO DATOS

datos$sales <- as.factor(datos$sales)
datos$salary <- as.factor(datos$salary)

datosin <- dummyVars("~.",data=datos, fullRank = F)
datos <- as.data.frame(predict(datosin,newdata=datos))

datos <- datos %>% 
  select(-c(sales.hr, salary.low)) %>% 
  mutate(left = as.factor(left))

datos$Work_accident <- datos$Work_accident
datos$promotion_last_5years <- datos$promotion_last_5years

balance <- table(datos$left)
prop.table(balance)

# Partir base en TRAIN y TEST

set.seed(30459) 
sample <- sample.int(nrow(datos), floor(.75*nrow(datos)))
datos.train <- datos[sample, ]
datos.test <- datos[-sample, ]

# ENTRENAR MODELO Y PASAR POR STEPWISE

modelo.logit <- glm(left~., family=binomial, datos.train)
steplogit <- step(modelo.logit, direction="both", trace=0)
summary(steplogit)

# INTERPRETAR ODDS

coeficiente <- modelo.logit$coefficients
ODD_ratios <- exp(coeficiente)
ODD_base <- prop.table(balance)[2]

ODD_final <- ODD_ratios*ODD_base

PROB_final <- ODD_final/(1+ODD_final)
PROB_final

# CARGAR DATOS NUEVOS

datos_pred <- read_excel("Datos_HR.xlsx", sheet= "Hoja1")

# PROCESAMIENTO DATOS NUEVOS

datos_pred$sales <- as.factor(datos_pred$sales)
datos_pred<-datos_pred[,1:ncol(datos_pred)-1]

datosin <- dummyVars("~.",data=datos_pred, fullRank = F)
datos_pred <- as.data.frame(predict(datosin,newdata=datos_pred))

datos_pred <- datos_pred %>% 
  select(-c(sales.hr))

datos_pred["salary.medium"]=1
datos_pred["salary.high"]=0
datos_pred["sales.IT"]=0
datos_pred["sales.management"]=0
datos_pred["sales.marketing"]=0
datos_pred["sales.product_mng"]=0
datos_pred["sales.RandD"]=0
datos_pred["sales.support"]=0
datos_pred["sales.technical"]=0

# PREDECIR LOG(ODD)

log_ODD<- predict(modelo.logit,datos_pred)
ODD <- exp(log_ODD)
prob_final_final <- ODD/(1+ODD)
prob_final_final
# PREDECIR PROBABILIDAD

predict(modelo.logit,datos_pred, type="response")


