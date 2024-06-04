if (!require('readxl')) install.packages('readxl')
if (!require('caret')) install.packages('caret')
if (!require('MLmetrics')) install.packages('MLmetrics')
if (!require('ROCR')) install.packages('ROCR')
if (!require('tidyverse')) install.packages('tidyverse')


# KNN ---------------------------------------------------------------------

# Lectura del archivo cartera 
getwd()

cartera <- read_excel("carteraguia2017.xlsx")
cartera %>% glimpse()

# Cambiar a tipo factor la variable tipo_ips
cartera <- cartera %>% 
  mutate(TIPO_ips = as.factor(TIPO_ips)) %>% 
  relocate(TIPO_ips, .after = cartera_actual_insumos_B)
cartera %>% glimpse()

# Crea un set completo de variables dummy
carterawin <- dummyVars("~.",data=cartera)
carterafin <- as.data.frame(predict(carterawin,newdata=cartera))

# Convertir y organizar como factor los retrasos
carterafin2 <- carterafin %>% 
  mutate(retrasos = as.factor(retrasos),
         retrasos = factor(retrasos, ordered = TRUE,
                           levels = c("1", "0")))

# Estandarización de las variables escalares
carterafin2 <- carterafin2 %>% 
  mutate_at(1:7, scale)
# carterafin2[1:7] <- scale(carterafin2[1:7])

set.seed(1545867) 

# Haciendo la partición de los datos

sample <- sample.int(nrow(carterafin2), floor(.75*nrow(carterafin2)))
cartera.train <- carterafin2[sample, ]
cartera.test <- carterafin2[-sample, ]

# Parámetros de validación cruzada
cross <- trainControl(method = "cv", number = 10)
modeloknn1 <- train(retrasos~., method = "knn",
                    tuneGrid = expand.grid(k = 1:30),
                    trControl = cross, 
                    metric = "Accuracy",
                    data = cartera.train)
modeloknn1
plot(modeloknn1)
resultados <- modeloknn1$results


levels(cartera.train$retrasos) <- make.names(levels(factor(cartera.train$retrasos)))

# Parámetros de validación cruzada 
cross <- trainControl(method = "cv", number = 5,
                      classProbs = TRUE,
                      summaryFunction = twoClassSummary)
modeloknn2 <- train(retrasos~., method = "knn",
                    tuneGrid = expand.grid(k=1:30),
                    trControl = cross, 
                    metric = "ROC",
                    data = cartera.train)
modeloknn2
plot(modeloknn2)
modeloknn2$results

# Desempeño del modelo sintonizado en test
predmod1 <- predict(modeloknn2, cartera.test, type = "prob")
pronknn1 <- ifelse(predmod1$X1 > 0.5, 1, 0)
pronknn1 <- factor(pronknn1, ordered=TRUE, levels = c("1","0"))
confknn1 <- confusionMatrix(as.factor(pronknn1),
                          cartera.test$retrasos, positive = "1")
confknn1$table
confknn1$byClass

# Curva ROC
# Objeto de predicciones y de la curva
pr <- prediction(as.numeric(pronknn1), 
                 as.numeric(cartera.test$retrasos))
curvaROC <- performance(pr, measure = "tpr", x.measure = "fpr")
# Curva
plot(curvaROC)
# Calcular el AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
# Ver el AUC
auc


# El modelo con un k escogido
modeloknn3 <- train(retrasos~., method = "knn",
                    tuneGrid = expand.grid(k = 8:8),
                    trControl = cross, 
                    metric = "ROC",
                    data = cartera.train)
modeloknn3$results

# Desempeño del modelo en test
predmod2 <- predict(modeloknn3, cartera.test, type = "prob")
pronknn2 <- ifelse(predmod2$X1 > 0.5, 1, 0)
pronknn2 <- factor(pronknn2, ordered=TRUE, levels = c("1","0"))

confknn2 <- confusionMatrix(as.factor(pronknn2),
                            cartera.test$retrasos, positive = "1")
confknn2$table
confknn2$byClass
confknn1$byClass


# Curva ROC
# Objeto de predicciones y de la curva
pr1 <- prediction(as.numeric(pronknn2), 
                  as.numeric(cartera.test$retrasos))
curvaROC1 <- performance(pr1, measure = "tpr", x.measure = "fpr")
# Curva
plot(curvaROC1)
# Calcular el AUC
auc1 <- performance(pr1, measure = "auc")
auc1 <- auc1@y.values[[1]]
# Ver el AUC
auc1


# Naive Bayes -------------------------------------------------------------
if (!require('e1071')) install.packages('e1071')
if (!require('caret')) install.packages('caret')
if (!require('ROCR')) install.packages('ROCR')

# Leer los datos
cereal <- read.csv("cereal2.csv", header = TRUE, sep = ";", dec = ",")
cereal %>% glimpse()

# Un pequeño arreglo
colnames(cereal)[1] <- "edadcat"

# Análisis de independencia
# H0: las variables son independientes
# H1: las variables son dependientes

chisq.test(table(cereal$genero, cereal$ecivil))
chisq.test(table(cereal$genero, cereal$activo))
chisq.test(table(cereal$genero, cereal$edadcat))
chisq.test(table(cereal$edadcat, cereal$ecivil))
chisq.test(table(cereal$edadcat, cereal$activo))
chisq.test(table(cereal$ecivil, cereal$activo))

# Haciendo la partición de la base
set.seed(19854)
sample <- sample.int(nrow(cereal), floor(.7*nrow(cereal)))
cereal.train <- cereal[sample, ]
cereal.test <- cereal[-sample, ]

prop.table(table(cereal.train$desayuno))

# Corriendo el algoritmo
naivetest <- naiveBayes(desayuno~., cereal.train)
naivetest

# Tasas base
naivetest$apriori
prop.table(naivetest$apriori)

# Probabilidades marginales
naivetest$tables$genero
naivetest$tables$edadcat
naivetest$tables$ecivil
naivetest$tables$activo

cereal.train %>% filter(desayuno == "Avena") %>% 
  count(edadcat)

cereal.test[1,]

# Guarda las probabilidades (para curva ROC)
prednaiveprob <- predict(naivetest, cereal.test, type = "raw")
prednaiveprob[1,]
# Guarda el pronóstico
# prednaive<-predict(naivetest, cereal.test)

prednaive <- colnames(prednaiveprob)[max.col(prednaiveprob, 
                                           ties.method = "first")]
prednaive %>% glimpse()

# Generar la matriz de confusión
naiveconf <- confusionMatrix(as.factor(prednaive),
                             as.factor(cereal.test$desayuno))
naiveconf$table
naiveconf$byClass

# AUC para la predicción de cada clase
# Crear resultados 1 o 0 para la predicción de Avena
avenas <- ifelse(cereal.test$desayuno == "Avena", 1, 0)

# Crear el objeto de predicción
predcor <- prediction(prednaiveprob[,1], avenas)

curvaROC <- performance(predcor, measure = "tpr", x.measure = "fpr")
# Curva
plot(curvaROC, main = "Curva ROC - Avena")
# Calcular el AUC
auc <- performance(predcor, measure = "auc")
auc <- auc@y.values[[1]]
# Ver el AUC
auc

# Crear resultados 1 o 0 para la predicción de cereal
cerealk <- ifelse(cereal.test$desayuno == "Cereales", 1, 0)

# Crear el objeto de predicción
predcor2 <- prediction(prednaiveprob[,3], cerealk)

curvaROC2 <- performance(predcor2, measure = "tpr", x.measure = "fpr")
# Curva
plot(curvaROC2, main = "Curva ROC - Cereal")
# Calcular el AUC
auc2 <- performance(predcor2, measure = "auc")
auc2 <- auc2@y.values[[1]]
# Ver el AUC
auc2

# Análisis de cutoffs

# Guardar los recall y puntos de corte
tablafinan <- performance(predcor2, measure = "rec")
cutoffs <- unlist(tablafinan@x.values)
recalls <- unlist(tablafinan@y.values)
# Guardar las precisiones
tablafina2 <- performance(predcor2, measure = "prec")
precisions <- unlist(tablafina2@y.values)
# Guardar F scores
tablafina3 <- performance(predcor2, measure = "f")
fscores <- unlist(tablafina3@y.values)
# Crear la tabla conjunta
tablacruce <- as.data.frame(cbind(cutoffs, precisions, recalls, fscores))
# Ver precisiones versus recall
tablacruce %>% ggplot(aes(x = precisions, y = recalls)) +
  geom_point() +
  theme_minimal()
# Ver el cutoff que maximiza F score
tablacruce %>% ggplot(aes(x = cutoffs, y = fscores)) +
  geom_point() + 
  theme_minimal()

tablacruce[which.max(tablacruce$fscores),]

# Calcular las ganancias
tabla <- table(cereal.test$desayuno)
prop.table(tabla)[3]

tablacruce <- tablacruce %>% 
  mutate(wins = prop.table(tabla)[3]*recalls*(250-(100/precisions)))
# Pintar las ganancias versus los cutoffs
tablacruce %>% ggplot(aes(x = cutoffs, y = wins)) +
  geom_point() +
  theme_minimal()

# Ver las ganancias frente a la precisión
tablacruce %>% ggplot(aes(x = precisions, y = wins)) +
  geom_point() +
  theme_minimal()

# Cutoff donde la ganancia es máxima
tablacruce[which.max(tablacruce$wins),]
