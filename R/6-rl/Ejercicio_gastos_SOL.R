library(readxl)

datos_train <- read_excel("C:/Users/aulasingenieria/Downloads/ejercicio gastos familia.xlsx", sheet="Train")
datos_test <- read_excel("C:/Users/aulasingenieria/Downloads/ejercicio gastos familia.xlsx", sheet="Test")


datos_train <- datos_train[,-1]
datos_test <- datos_test[,-1]

matrizcor <- cor(datos_train)
corrplot(matrizcor, method="square", tl.cex = 0.7,col=brewer.pal(n=8, name="PuOr"),addCoef.col = "black", 
         number.cex=0.7,type = "upper", diag = FALSE)



colnames(datos_train)<- c("Gastos_comida","Ingresos","Tam_familia","Hijos_universidad")

colnames(datos_test)<- c("Gastos_comida","Ingresos","Tam_familia","Hijos_universidad")

modelo_1 <- lm(Gastos_comida ~., data=datos_train)

summary(modelo_1)

modelo_2 <- lm(Gastos_comida ~ Ingresos, data=datos_train)

summary(modelo_2)

AIC(modelo_1)
AIC(modelo_2)



pred1 <- predict(modelo_1,datos_test, se.fit = TRUE)

pred2 <- predict(modelo_2,datos_test)

RMSE1 <- sqrt(mean((pred1-datos_test$Gastos_comida)^2))
RMSE2 <- sqrt(mean((pred2-datos_test$Gastos_comida)^2))
RMSE1
RMSE2

