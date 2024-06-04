if (!require('readxl')) install.packages('readxl')
if (!require('caret')) install.packages('caret')
if (!require('ROCR')) install.packages('ROCR')
if (!require('lift')) install.packages('lift')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('MLmetrics')) install.packages('MLmetrics')

## Lectura del archivo cartera 
## Revisar que el archivo cartera se encuentre en documentos, 
## o la ruta en donde se encuentra direccionado R

getwd()

cartera <- read_excel("carteraguia2017.xlsx")
cartera %>% glimpse()

cartera <- cartera %>% 
  mutate(TIPO_ips = as.factor(TIPO_ips))

# Creación de dummy para la IPS
# (no es necesario pues la función glm lo hace automático)
carterawin <- dummyVars("~.",data=cartera, fullRank = F)
carterafin <- as.data.frame(predict(carterawin,newdata=cartera))
carterafin2 <- carterafin %>% 
  select(-c(TIPO_ips.5)) %>% 
  mutate(retrasos = as.factor(retrasos))

carterafin2 %>% glimpse()

# Exploración variable de respuesta
balance <- table(cartera$retrasos)
prop.table(balance)

# Partición de la base
set.seed(30459) 
# Aquí se define el tamaño de la muestra, en este caso entrenamiento tendrá el 75% de los casos
sample <- sample.int(nrow(carterafin2), floor(.75*nrow(carterafin2)))
cartera.train <- carterafin2[sample, ]
cartera.test <- carterafin2[-sample, ]

## Modelo
modelo.logit <- glm(retrasos~., family=binomial, cartera.train)
# trace=0 impide ver todos los detalles de la optimización stepwise
steplogit <- step(modelo.logit, direction="both", trace=0)
summary(steplogit)

coeficientes <- steplogit$coefficients
odd_change <- exp(coeficientes)
odd_change

# Odd ratios
oddbase<-prop.table(balance)[2]/prop.table(balance)[1]
oddbase

oddfin <- oddbase*odd_change
prob1step <- oddfin/(1+oddfin)
prob1step

# Análisis de no linealidad de los incrementos
# Crear matriz vacía
oddprueba <- matrix(,6,29)
probprueba <- matrix(,6,29)

# Borrar el intercepto
odd_change2 <- odd_change[2:7]

# Guardar odd base y probabilidad base
oddprueba[,11] <- oddbase
probprueba[,11] <- oddprueba[,11]/(1+oddprueba[,11])

# Calcular odds y probabilidades 17 pasos adelante
for (i in 12:29){ 
  oddprueba[,i] <- oddprueba[,i-1]*odd_change2
  probprueba[,i] <- oddprueba[,i]/(1+oddprueba[,i])
}
# y 10 atrás
for (i in 10:1){ 
  oddprueba[,i] <- oddprueba[,i+1]/odd_change2
  probprueba[,i] <- oddprueba[,i]/(1+oddprueba[,i])
}

# Guardar en un dataframe
probver <- as.data.frame(t(probprueba))
# Crear id y graficar: cartera insumos A
probver$id <- seq(1:29)-11
names(probver) <- c("Tipo IPS 1", "Tipo IPS 4", "años igual dueño",
                    "años dirección actual", "%pasivo/ venta anual",
                    "cartera insumos A", "pasos")
plot(probver$pasos, probver$`cartera insumos A`)

# Veamos las predicciones en la base de entrenamiento
cartera.train_2 <- cartera.train %>% 
  mutate(probabilidades = steplogit$fitted.values)

# Crea el pronóstico base de entrenamiento
prontrain <- ifelse(steplogit$fitted.values > 0.5,1,0)

# Matriz de confusión y estadísticas, base de entrenamiento
conftrain <- confusionMatrix(as.factor(prontrain),
                             cartera.train$retrasos, positive = "1")
conftrain$table
conftrain$byClass

# Crea el pronóstico en base de prueba
probtest <- predict(steplogit, newdata = cartera.test, type='response')
test_con_prob <- cartera.test %>% 
  mutate(probabilidades = probtest)

# write.csv(test_con_prob, "prueba_prob.csv")

prontest <- ifelse(probtest > 0.5,1,0)
conftest <- confusionMatrix(as.factor(prontest),cartera.test$retrasos, 
                            positive = "1")
conftest$table
conftest$byClass

# F-beta score
FBeta_Score(cartera.test$retrasos, prontest, positive = "1", beta = 1)
FBeta_Score(cartera.test$retrasos, prontest, positive = "1", beta = 0.5)
FBeta_Score(cartera.test$retrasos, prontest, positive = "1", beta = 2)

# Crear objeto de predicciones
pr <- prediction(probtest, cartera.test$retrasos)
pr@predictions
# Cutoffs vs precisión
cut_prec <- performance(pr, measure = "prec")
plot(cut_prec)
# Cutoffs vs recall
cut_rec <- performance(pr, measure = "tpr")
plot(cut_rec)

# Curva ROC
curvaROC <- performance(pr, measure="tpr", x.measure="fpr")
# Gráfico de la curva
plot(curvaROC)
abline(h = 0.8)
# Calcular el AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
# Ver el AUC
auc


# Extrayendo las coordenadas de la curva ROC
fprs <- unlist(curvaROC@x.values)
tprs <- unlist(curvaROC@y.values)
tablaCOR <- as.data.frame(cbind(fprs,tprs))
tablaCOR[65:95,]


# library("lift")
# Gráfico de lift
plotLift(probtest, cartera.test$retrasos, cumulative = FALSE)
abline(h = 1) # El lift que se tendr?a sin modelo

# lift acumulado
plotLift(probtest,cartera.test$retrasos, cumulative = TRUE)

gain<-performance(pr, "tpr", "rpp")
plot(gain)
abline(h = 0.8)

# Guardar las exhaustividades y puntos de corte
tablafinan <- performance(pr, measure="rec")
cutoffs <- unlist(tablafinan@x.values)
recalls <- unlist(tablafinan@y.values)
# Guardar las precisiones
tablafina2 <- performance(pr, measure="prec")
precisions <- unlist(tablafina2@y.values)
# Crear la tabla conjunta
tablacruce <- as.data.frame(cbind(cutoffs,precisions,recalls))
# Ver precisiones versus exhaustividad
tablacruce %>% ggplot(aes(x = precisions, y = recalls)) +
  geom_point() +
  theme_minimal()

# Ver cutoffs vs precisiones y recalls
tablacruce %>% ggplot() +
  geom_point(aes(x = cutoffs, y = precisions)) +
  geom_point(aes(x = cutoffs, y = recalls), color="darkred") +
  theme_minimal()


# Calcular las ganancias
tablacruce <- tablacruce %>% 
  mutate(wins = prop.table(balance)[2]*recalls*(500-(100/precisions)))


# Pintar las ganancias versus los cutoffs
tablacruce %>% 
  ggplot(aes(x = cutoffs, y = wins)) + 
  geom_point() +
  theme_minimal()

# Ver las ganancias frente a la precisión
tablacruce %>% 
  ggplot(aes(x = precisions, y = wins)) + 
  geom_point() +
  theme_minimal()

# Cutoff donde la ganancia es máxima
tablacruce[which.max(tablacruce$wins),]

# Balanceo
## separamos ceros y unos
no_retraso <- carterafin2 %>% 
  filter(retrasos == 0)

si_retraso <- carterafin2 %>% 
  filter(retrasos == 1)

# En ambas bases el 20% es prueba
set.seed(24029114)
# 20% de ceros
muestra <- sample.int(nrow(si_retraso), round(.2*nrow(si_retraso)))
si_retraso_test <- si_retraso[muestra, ]
si_retraso_resto <- si_retraso[-muestra, ]
# 20% de unos
muestra2 <- sample.int(nrow(no_retraso), round(.2*nrow(no_retraso)))
no_retraso_test <- no_retraso[muestra2, ]
no_retraso_resto <- no_retraso[-muestra2, ]

# Unir las dos
cart.test <- rbind(si_retraso_test, no_retraso_test)
nrow(cart.test)
prop.table(table(cart.test$retrasos))

muestra3 <- sample.int(nrow(si_retraso_resto), round(.5*nrow(si_retraso_resto)))
si_retraso_train <- si_retraso_resto[muestra3, ]
si_retraso_valid <- si_retraso_resto[-muestra3, ]

# Pongo la misma cantidad de ceros y unos en entrenamiento: 
muestra4 <- sample.int(nrow(no_retraso_resto), nrow(si_retraso_train))
no_retraso_train <- no_retraso_resto[muestra4, ]
no_retraso_valid <- no_retraso_resto[-muestra4, ]
# Creo entrenamiento y validaci?n
cart.train <- rbind(si_retraso_train, no_retraso_train)
cart.valid <- rbind(si_retraso_valid, no_retraso_valid)
# Chequeo que todo va bien
table(cart.train$retrasos)
table(cart.valid$retrasos)
table(cart.test$retrasos)
prop.table(table(cart.train$retrasos))
prop.table(table(cart.valid$retrasos))
prop.table(table(cart.test$retrasos))
prop.table(table(carterafin2$retrasos)) #proporciones originales


cart.train %>% ggplot(aes(x = `IGUAL_DUENO`, 
                          y = cartera_actual_insumos_A,
                          color = retrasos))+
  geom_point()+
  theme_minimal()


modelo.balance <- glm(retrasos~., family=binomial, cart.train)
stepbalance <- step(modelo.balance, direction="both", trace=0)
summary(stepbalance)

coeficientes2 <- stepbalance$coefficients
odd_changeb <- exp(coeficientes2)
odd_changeb


# Crea el pronóstico en prueba
probtest2 <- predict(stepbalance, newdata = cart.test, type='response')
prontest2 <- ifelse(probtest2 > 0.5, 1, 0)
conftest2 <- confusionMatrix(as.factor(prontest2),
                             cart.test$retrasos, positive = "1")
conftest2$table
conftest2$byClass

# Crear objeto de predicciones
pr2 <- prediction(probtest2, cart.test$retrasos)
# Creación del objeto de la curva
curvaROC2 <- performance(pr2, measure="tpr", x.measure="fpr")
# Gráfico de la curva
plot(curvaROC2)
abline(h = 0.8)
# Calcular el AUC
auc2 <- performance(pr2,measure = "auc")
auc2f <- auc2@y.values[[1]]
# Ver el AUC
auc2f

# Validación cruzada (sin balanceo)
datos_train <- rbind(cart.train, cart.valid)

train_control <- trainControl(method = "cv", number = 10)

# train the model on training set
model <- train(retrasos ~ .,
               data = datos_train,
               trControl = train_control,
               method = "glm",
               family = binomial())

# print cv scores
summary(model)

pred_test_cv <- predict(model, newdata = cart.test, type='prob')
pron_test_cv <- ifelse(pred_test_cv$`1` > 0.5, 1, 0)
conf_test_cv <- confusionMatrix(as.factor(pron_test_cv),
                             cart.test$retrasos, positive = "1")
conf_test_cv$table
conf_test_cv$byClass

# Crear objeto de predicciones
pred_cv <- prediction(pred_test_cv$`1`, cart.test$retrasos)
# Creación del objeto de la curva
curvaROC_cv <- performance(pred_cv, measure="tpr", x.measure="fpr")
# Gráfico de la curva
plot(curvaROC_cv)
abline(h = 0.8)
# Calcular el AUC
auc_cv <- performance(pred_cv, measure = "auc")
auc_cv1 <- auc_cv@y.values[[1]]
# Ver el AUC
auc_cv1

