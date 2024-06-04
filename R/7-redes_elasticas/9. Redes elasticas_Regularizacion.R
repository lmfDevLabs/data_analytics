if (!require('caret')) install.packages('caret')
if (!require('corrplot')) install.packages('corrplot')
if (!require('RColorBrewer')) install.packages('RColorBrewer')
if (!require('glmnet')) install.packages('glmnet')
if (!require('corrr')) install.packages('corrr')
if (!require('Metrics')) install.packages('Metrics')
if (!require('tidyverse')) install.packages('tidyverse')

#Lectura e inspección de estructura de los datos

getwd()
carros <- read.csv("carros2011imputado.csv", header=TRUE, sep=";", dec=",")
carros %>% glimpse()

carros %>% colnames()

#guardar los modelos de los carros para interpretación
carmodelos <- carros %>%  
  select(modelo)
#remover el ID, el modelo y dos de los precios
carros <- carros %>% 
  select(-c(ID, modelo, precio_basico, precio_equipado))
carros %>% dim()
carros %>% View()

#crear variables dummy (binarias)
carroswin <- dummyVars(~.,data=carros)
carrosfin <- as.data.frame(predict(carroswin,newdata=carros))
carrosfin %>% View()

#Eliminar una categoría por variable
carrosfin2 <- carrosfin %>% 
  select(-c(fabricanteLexus, tipoSporty, tracciónTrasera,transmisión_manualSi,
            `numero_de_airbagsNo tiene`, `hecho_o_no_en_USAHecho fuera de USA`))

carrosfin2 %>% View()

#veamos la dimension final de la base de datos
carrosfin2 %>% dim()

#retiro la variable a predecir
predictores <- carrosfin2 %>% 
  select(-precio_promedio)

#Gráfico de correlaciones
corpred <- cor(predictores)
corrplot(corpred, method="square",tl.cex = 0.7,col=brewer.pal(n=8, name="PuOr"),
         type = "upper", tl.pos = 'n')

#Filtrando variables altamente correlacionadas
corfil <- predictores %>% 
  correlate(use = "pairwise.complete.obs") %>% 
  shave() %>% 
  stretch(na.rm = TRUE) %>% 
  filter(between(r, 0.6, 1)|between(r,-1,-0.6))
corfil %>% View()

#Componentes principales
componentescar <- prcomp(predictores, center=TRUE, scale.=TRUE)
summary(componentescar)

##Entrenamiento y validación 80-20
set.seed(49584) 
sample <- sample.int(nrow(carrosfin2), floor(.8*nrow(carrosfin2)))
carros.train <- carrosfin2[sample, ]
carros.test <- carrosfin2[-sample, ]

# haciendo el modelo sencillo y el step
modeloaug <- lm(precio_promedio~., data=carros.train)
modelocarstep <- step(modeloaug, direction="both", trace=0)
summary(modelocarstep)

# Separando predictoras en las bases

predic.train <- carros.train %>% 
  select(-precio_promedio) %>% 
  as.matrix()

precio.train <- carros.train %>% 
  select(precio_promedio) %>% 
  as.matrix()

predic.test <- carros.test %>% 
  select(-precio_promedio) %>% 
  as.matrix()

precio.test <- carros.test %>% 
  select(precio_promedio) %>% 
  as.matrix()

# Corriendo los modelos de regularización
fitlasso <- glmnet(predic.train,precio.train,alpha = 1)
fitridge <- glmnet(predic.train,precio.train,alpha = 0)

#ver la variacion de los coeficientes con el lambda
plot(fitlasso, xvar="lambda")
plot(fitridge, xvar="lambda")

#------------------------------------
# Validación cruzada
#------------------------------------

#Ridge
foundridge<-cv.glmnet(predic.train, 
                      precio.train,
                      alpha=0,
                      nfolds=5)

# Lambdas
foundridge$lambda

# Errores promedio de validación cruzada
foundridge$cvm

# Estimación de errores estándar de cvm
foundridge$cvsd

# cvm mínimo
min(foundridge$cvm)

# Límites
foundridge$cvm[1]+foundridge$cvsd[1]
foundridge$cvm[1]-foundridge$cvsd[1]
foundridge$cvup[1]
foundridge$cvlo[1]

#gráfico lambda vs mse
plot(foundridge)
abline(h=c(min(foundridge$cvm),foundridge$cvup[which(foundridge$lambda==foundridge$lambda.min)]), 
       lty=c(2,2), col=c("blue", "blue"))

#veo los lambdas mínimo y 1se
foundridge$lambda.1se #Lambda más grande cuyo valor está dentro de un error estandar 
                      #desde mínimo
foundridge$lambda.min

#veamos los logaritmos
log(foundridge$lambda.min)
log(foundridge$lambda.1se)

# Coeficientes para lambda.1se
coef(foundridge,s=foundridge$lambda.1se)

# Lasso
foundlasso <- cv.glmnet(predic.train, 
                        precio.train,
                        alpha=1,
                        nfolds=5)

#gráfico lambda vs mse
plot(foundlasso)

#veo los lambdas mínimo y 1se
foundlasso$lambda.1se
foundlasso$lambda.min

#logaritmos
log(foundlasso$lambda.min)
log(foundlasso$lambda.1se)

#Fila que corresponde al lambda.min
which(foundlasso$lambda==foundlasso$lambda.min)
foundlasso$cvup[which(foundlasso$lambda==foundlasso$lambda.min)]

#Fila que corresponde al lambda.1se
which(foundlasso$lambda==foundlasso$lambda.1se)
foundlasso$cvm[which(foundlasso$lambda==foundlasso$lambda.1se)]

#Cantidad de coeficientes no nulos para cada lambda
foundlasso$nzero[which(foundlasso$lambda==foundlasso$lambda.min)]
foundlasso$nzero[which(foundlasso$lambda==foundlasso$lambda.1se)]

# Coeficientes para lambda.min
coef(fitlasso,s=foundlasso$lambda.min)
# Coeficientes para lambda.1se
coef(fitlasso, s=foundlasso$lambda.1se)

#------------------------------------
# Redes elásticas
#------------------------------------

# aquí se hacen redes elásticas de 0.1 a 0.9
for (i in 1:9){ 
  assign(paste("found", i, sep=""), cv.glmnet(predic.train, precio.train, nfolds=5, 
                                              alpha=i/10))
}

#obtengo los valores del mínimo mse

redes <- tibble(
  i = 0:10,
  alfa = i/10,
  min_cvm = c(min(foundridge$cvm), min(found1$cvm), min(found2$cvm),
              min(found3$cvm), min(found4$cvm), min(found5$cvm),
              min(found6$cvm), min(found7$cvm), min(found8$cvm),
              min(found9$cvm), min(foundlasso$cvm))
)

redes


#Corro el modelo con la red de menor cvm
elastic7 <- glmnet(predic.train,precio.train,alpha = 0.7)
coef(elastic7,s=found7$lambda.min)
coef(elastic7,s=found7$lambda.1se)

# Predicciones sobre el test set
# Red elástica
pred_red <- predict.glmnet(elastic7, predic.test, s=found7$lambda.min)
pred_red_se <- predict.glmnet(elastic7, predic.test, s=found7$lambda.1se)
# Ridge
pred_ridge <- predict.glmnet(fitridge, predic.test, s=foundridge$lambda.min)
# Lasso
pred_lasso <- predict.glmnet(fitlasso, predic.test, s=foundlasso$lambda.min)
# Stepwise
pred_step <- predict(modelocarstep,carros.test)

# Cálculo  del RMSE para cada modelo
rmseelastic <- rmse(precio.test[,1],pred_red)
rmseelastic_se <- rmse(precio.test[,1],pred_red_se)
rmseridge <- rmse(precio.test[,1],pred_ridge)
rmselasso <- rmse(precio.test[,1],pred_lasso)
rmsestep <- rmse(precio.test[,1],pred_step)

rmseelastic
rmseelastic_se
rmseridge
rmselasso
rmsestep

1000*(rmseelastic_se-rmseridge)
(rmseelastic_se-rmseridge/rmseridge)
