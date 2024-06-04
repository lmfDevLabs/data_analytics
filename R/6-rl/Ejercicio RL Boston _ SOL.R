if (!require('corrplot')) install.packages('corrplot')
if (!require('lmtest')) install.packages('lmtest')
if (!require('MASS')) install.packages('MASS')
if (!require('leaps')) install.packages('leaps')
if (!require('RColorBrewer')) install.packages('RColorBrewer')
if (!require('Metrics')) install.packages('Metrics')
if (!require('forecast')) install.packages('forecast')
if (!require('tidyverse')) install.packages('tidyverse')

if (!require('caret')) install.packages('caret')
if (!require('corrplot')) install.packages('corrplot')
if (!require('RColorBrewer')) install.packages('RColorBrewer')
if (!require('glmnet')) install.packages('glmnet')
if (!require('corrr')) install.packages('corrr')
if (!require('Metrics')) install.packages('Metrics')
if (!require('tidyverse')) install.packages('tidyverse')



# PAQUETE MASS. Datos Boston. Recoje la mediana del valor de vivienda para 506 zonas con 
# la siguiente informaci?n adicional:

# crim: ratio de criminalidad per c?pita de cada ciudad.
# zn: Proporci?n de zonas residenciales con edificaciones de m?s de 25.000 pies cuadrados.
# indus: proporci?n de zona industrializada.
# chas: Si hay r?o en la ciudad (= 1 si hay r?o; 0 no hay).
# nox: Concentraci?n de ?xidos de nitr?geno (partes per 10 mill?n).
# rm: promedio de habitaciones por vivienda.
# age: Proporci?n de viviendas ocupadas por el propietario construidas antes de 1940.
# dis: Media ponderada de la distancias a cinco centros de empleo de Boston.
# rad: ?ndice de accesibilidad a las autopistas radiales.
# tax: Tasa de impuesto a la propiedad en unidades de $10,000.
# ptratio: ratio de alumnos/profesor por ciudad.
# black: 1000(Bk - 0.63)^2 donde Bk es la proporci?n de gente de color por ciudad.
# lstat: porcentaje de poblaci?n en condici?n de pobreza.
# medv: Valor mediano de las casas ocupadas por el due?o en unidades de $1000s.

library(MASS)
data("Boston")

#LIMPIAR BASE

datos <- Boston[,-12]

#EXPLORACION BASE

glimpse(datos)

matrizcor <- cor(datos)
corrplot(matrizcor, method="square", tl.cex = 0.7,col=brewer.pal(n=8, name="PuOr"),addCoef.col = "black", 
         number.cex=0.7,type = "upper", diag = FALSE)


boxplot(datos[,9:13])


#ESTANDARIZAR


var_chas <- datos$chas

datos_esc <- scale(datos[,-4])

datos_esc <- as.data.frame(cbind(datos_esc, var_chas))

# EJEMPLO ESTANDARIZACION MANUAL

#est_manual <- (datos$crim - mean(datos$crim) ) / sd(datos$crim)

#datos_esc$crim - est_manual

prom <- lapply(datos, mean)
sd <- lapply(datos, sd)

#PARTICION BASE

#particion aleatoria de un porcentaje - 80%
#sample <- sample.int(nrow(datos_esc), floor(.8*nrow(datos_esc)))
#datos.train <- datos_esc[sample, ]
#datos.test <- datos_esc[-sample, ]

datos.train <- datos_esc[1:400,]
datos.test <- datos_esc[401:506, ]

###############################################################
########################  TRAIN ###############################
###############################################################


#REGRESION LINEAL TODO

modelo_simple <- lm(medv~., datos.train)

summary(modelo_simple)

#REGRESION STEP

modelostep <- step(modelo_simple,direction="both", trace=0)
summary(modelostep)

#REGRESION COMP PRINCIPALES

#datos_esc_x <- datos_esc[,-12]

datos_esc_x <- datos_esc %>%select(-medv)
datos_esc_y <- datos_esc %>%select(medv)

pcomp <- prcomp(datos_esc_x)
summary(pcomp)

datos_pc <- as.data.frame(pcomp$x)

datos_pc <- datos_pc[,1:4]

datos_pc <- cbind(datos_pc, datos_esc_y)

datos_pc.train <- datos_pc[1:400,] 
datos_pc.test <- datos_pc[401:506,]

modelo_pc <- lm(medv~., datos_pc.train)

summary(modelo_pc)

#REGRESION RIDGE

x.train<- datos.train %>% 
  select(-medv) %>% 
  as.matrix()
y.train<-datos.train %>% 
  select(medv) %>% 
  as.matrix()

#Ridge
foundridge<-cv.glmnet(x.train, 
                      y.train,
                      alpha=0,
                      nfolds=5)


coef(foundridge,s=foundridge$lambda.1se)

ridge <- glmnet(x.train,y.train,alpha = 0)

#REGRESION LASSO

foundlasso<-cv.glmnet(x.train, 
                      y.train,
                      alpha=1,
                      nfolds=5)


coef(foundlasso,s=foundlasso$lambda.1se)

#REGRESION ELASTICA 50-50

foundelastic<-cv.glmnet(x.train, 
                      y.train,
                      alpha=0.5,
                      nfolds=5)


coef(foundelastic,s=foundelastic$lambda.1se)


#####################################################################
#######################  TEST #######################################
#####################################################################


#MODELO SIMPLE
y_test <- datos.test  %>% select(medv) %>% as.matrix()

x_test <- datos.test %>% select(-medv) %>% as.matrix()

pred_simple <- predict(modelo, x_test)

y_pred_simple <- (pred_simple*sd$medv) + prom$medv
y_test_orig <- (y_test*sd$medv) + prom$medv

y_test_orig <- y_test_orig %>% as.matrix()
y_pred_simple <- y_pred_simple %>% as.matrix()


View(cbind(y_test_orig, y_pred_simple))

RMSE_simple <- sqrt(mean((y_test_orig-y_pred_simple)^2))


#MODELO STEP

pred_step <- predict(modelostep, x_test)

y_pred_step <- (pred_step*sd$medv) + prom$medv
y_test_orig <- (y_test*sd$medv) + prom$medv

y_test_orig <- y_test_orig %>% as.matrix()
y_pred_step <- y_pred_step %>% as.matrix()


View(cbind(y_test_orig, y_pred_step))

RMSE_step <- sqrt(mean((y_test_orig-y_pred_step)^2))


#MODELO PCOMP

y_test <- datos_pc.test  %>% select(medv)

x_test <- datos_pc.test  %>% select(-medv)

pred_pc <- predict(modelo_pc, x_test)

y_pred_pc <- (pred_pc*sd$medv) + prom$medv
y_test_orig <- (y_test*sd$medv) + prom$medv

y_test_orig <- y_test_orig %>% as.matrix()
y_pred_pc <- y_pred_pc %>% as.matrix()


View(cbind(y_test_orig, y_pred_pc))

RMSE_pc <- sqrt(mean((y_test_orig-y_pred_pc)^2))

#MODELO RIDGE


pred_rid_se <- predict.glmnet(ridge, x_test, s=foundridge$lambda.1se)
y_pred_rid <- (pred_rid_se*sd$medv) + prom$medv
y_test_orig <- y_test_orig %>% as.matrix()
y_pred_rid <- y_pred_rid %>% as.matrix()


View(cbind(y_test_orig, y_pred_rid))

RMSE_ridge <- sqrt(mean((y_test_orig-y_pred_rid)^2))


RMSE_simple
RMSE_step
RMSE_pc
RMSE_ridge





