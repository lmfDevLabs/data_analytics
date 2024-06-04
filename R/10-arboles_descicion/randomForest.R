

library(randomForest)

data(iris)

datos <- iris


# entrenar un arbol de decision que pronostique la especie de las plantas. 
# se puede partir train y test como se desee
# Reportar Accuracy

sample <- sample.int(nrow(datos), floor(.75*nrow(datos)))
iris.train <- datos[sample, ]
iris.test <- datos[-sample, ]


bosque <- randomForest(Species~., data=iris.train)

pr <- predict(bosque, newdata = iris.test)

conf1 <- confusionMatrix(pr,iris.test$Species)

conf1$overall["Accuracy"]




