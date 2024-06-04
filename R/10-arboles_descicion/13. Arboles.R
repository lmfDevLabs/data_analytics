
if (!require('party')) install.packages('party')
if (!require('rpart')) install.packages('rpart')
if (!require('rpart.plot')) install.packages('rpart.plot')
if (!require('dplyr')) install.packages('dplyr')
if (!require('caret')) install.packages('caret')


getwd()

cerealtrain <- read.csv("cerealtrain.csv", header = TRUE, sep = ";", dec = ",")
cerealtest <- read.csv("cerealtest.csv", header = TRUE, sep = ";", dec = ",")

cerealtrain %>% glimpse()
cerealtest %>% glimpse()

#Convertir a factor
cerealtrain <- cerealtrain %>% 
  mutate_if(is.character, as.factor)

cerealtest <- cerealtest %>% 
  mutate_if(is.character, as.factor)

#Explorando
table(cerealtrain$desayuno)
prop.table(table(cerealtrain$desayuno))

cerealtrain %>% distinct(edadcat)

#Árbol
set.seed(3435)
arbol <- rpart(desayuno ~ ., data = cerealtrain)
rpart.plot(arbol) 
rpart.rules(arbol)

unique(cerealtrain$edadcat)

#Entendiendo el primer nodo
edad1 <- cerealtrain %>% 
  filter(edadcat == "46-60" | edadcat == "Más de 60")
prop.table(table(edad1$desayuno))
dim(edad1)[1]/dim(cerealtrain)[1]

#Resumen del árbol
summary(arbol)

# arbol$cptable
# cp = arbol$cptable[which.min(arbol$cptable[,"xerror"]),"CP"]

arbol$variable.importance

# Ver resultado del desempeño del árbol de acuerdo a cp por cada partición
cps <- printcp(arbol) 

table(cerealtrain$desayuno)
error_inicial <- (158+118)/(158+118+163)
error_inicial
misclassification_rate <- cps[,3]*error_inicial
misclassification_rate

# visualizar resultados cost complexity pruning 
plotcp(arbol)

#Poda
arbolPodado = prune(arbol, cp = 0.02)
rpart.plot(arbolPodado)
printcp(arbolPodado)

#EL árbol completo
ELarbol <-  rpart(desayuno ~ ., data = cerealtrain, cp = 0.0001)
rpart.plot(ELarbol)
printcp(ELarbol)

#Controlando otros parámetros
arbolnew <- rpart(desayuno ~ ., data = cerealtrain,
                  control = rpart.control(xval = 10, 
                                          minbucket = 5, 
                                          minisplit = 30, 
                                          cp = 0.0041))
rpart.plot(arbolnew)
summary(arbolnew)

#Limitando profundidad
arbolnew2 <- rpart(desayuno ~ ., data = cerealtrain,
                   control = rpart.control(maxdepth = 2))
rpart.plot(arbolnew2)

#CHAID
arbre <- ctree(desayuno~., data = cerealtrain)
plot(arbre)

#predicciones
pr <- predict(arbol, cerealtest, type = "class")
head(cerealtest)
head(pr)
pr1 <- predict(arbolPodado, cerealtest, type = "class")
pr2 <- predict(arbolnew, cerealtest, type = "class")
pr3 <- predict(arbolnew2, cerealtest, type = "class")
pr4 <- predict(ELarbol, cerealtest, type = "class")
prchaid <- predict(arbre, cerealtest, type = "response")


#matriz de confusión (clase mayoritaria)

conf1 <- confusionMatrix(pr,cerealtest$desayuno)
conf2 <- confusionMatrix(pr1,cerealtest$desayuno)
conf3 <- confusionMatrix(pr2,cerealtest$desayuno)
conf4 <- confusionMatrix(pr3,cerealtest$desayuno)
conf5 <- confusionMatrix(pr4,cerealtest$desayuno)
conf6 <- confusionMatrix(prchaid,cerealtest$desayuno)

conf1$byClass[,"F1"]
conf2$byClass[,"F1"]
conf3$byClass[,"F1"]
conf4$byClass[,"F1"]
conf5$byClass[,"F1"]
conf6$byClass[,"F1"]

#Prediciendo con cutoff
#Supongamos que nuestra clase de interés es Cereales

cerealtest$Cereal <- ifelse(cerealtest$desayuno == "Cereales", 1, 0)
cerealtest$Cereal <- as.factor(cerealtest$Cereal)
head(cerealtest,n = 12)

pr1a <- predict(arbol, cerealtest, type="prob")
pr1a <- as.data.frame(pr1a)
head(pr1a, n = 12)

#Cut off de 0.3
cerealtest$Cereal_pred <- ifelse(pr1a$Cereales > 0.3, 1, 0)
cerealtest$Cereal_pred <- as.factor(cerealtest$Cereal_pred)
head(cerealtest, n = 12)

#Matriz de confusión
matriz <- confusionMatrix(cerealtest$Cereal_pred, cerealtest$Cereal)
matriz$byClass["F1"]
matriz$table