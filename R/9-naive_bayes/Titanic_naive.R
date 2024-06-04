

datos_titanic <- read_excel("titanicdf - naive.xlsx", sheet="titanicdf")

str(datos_titanic)

datos_titanic$Survived <- as.factor(datos_titanic$Survived)
datos_titanic$Pclass <- as.factor(datos_titanic$Pclass)

#sample <- sample.int(nrow(cereal), floor(.7*nrow(cereal)))
datos.train <- datos_titanic
datos.test <- read_excel("titanicdf - naive.xlsx", sheet="Pasajeros nuevos ")

str(datos.test)

datos.test$Survived <- as.factor(datos.test$Survived)
datos.test$Pclass <- as.factor(datos.test$Pclass)


naivetest <- naiveBayes(Survived~., datos.train)
naivetest


prednaiveprob <- predict(naivetest, datos.test, type = "raw")

prednaivecat <- ifelse(prednaiveprob[,2] > 0.5, 1, 0)


# Generar la matriz de confusión
naiveconf <- confusionMatrix(as.factor(prednaivecat),
                             as.factor(datos.test$Survived))
naiveconf$table
naiveconf$byClass


(134+78)/(134+1+15+78)
