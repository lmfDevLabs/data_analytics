if (!require('corrplot')) install.packages('corrplot')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('psych')) install.packages('psych')
if (!require('RColorBrewer')) install.packages('RColorBrewer')
if (!require('factoextra')) install.packages('factoextra')
if (!require('reshape2')) install.packages('reshape2')
if (!require('FactoMineR')) install.packages('FactoMineR')
if (!require('readxl')) install.packages('readxl')


# Lectura de los datos que deben estar en la ruta que genera el siguiente código
getwd()


# Ejercicio 1 - Cartera ---------------------------------------------------

cartera <- read.csv("cartera.csv", header=TRUE, sep=";", dec=",")
cartera %>% glimpse()

# Se quitan las variables categóricas
carteraz <- cartera %>% 
  select(-c(tipoips, retrasos))

# quita<-c(-2,-9)
# carteraz<-cartera[,quita]

carteraz %>% glimpse()

#creo matriz de correlaciones
base <- cor(carteraz)
#ver la matriz redondeada a 4 decimales
round(base,4)

#gráfico de correlaciones
corrplot(base, type="upper", tl.cex = 0.7,col=brewer.pal(n=8, name="PuOr"))
corrplot(base, method="number",tl.cex = 0.7,col=brewer.pal(n=8, name="PuOr"), type = "upper")
corrplot(base, method="square",tl.cex = 0.7,col=brewer.pal(n=8, name="PuOr"),addCoef.col = "black", 
         number.cex=0.7,type = "upper", diag = FALSE, order="FPC")

#Test de Bartlett (prueba de esfericidad)
# H0: no hay correlación vs H1: sí hay correlación
#Si p-value < 0.05 entonces hay correlación significativa entre variables y se puede
#aplicar un ACP
# library(psych)
Test.Bartlett <- cortest.bartlett(base,n=nrow(carteraz))
Test.Bartlett

#Test KMO
KMO(base)

## Aplicamos componentes principales y se estandarizan los datos
componentestotal <- prcomp(carteraz, center=TRUE, scale.=TRUE)
summary(componentestotal)

#Variabilidad explicada en términos de cantidad de variables
lambdas <- as.data.frame(summary(componentestotal)[1])^2
lambdas

#Veamos la dirección de cada componente
print(componentestotal)

## Cuántos componentes se deben escoger - Gráfico de sedimentación
plot(componentestotal, type="l", main = "Gráfico de sedimentación", ylim=c(0,4))

# Gráfico usando el paquete factoextra
fviz_eig(componentestotal, geom="line", choice="eigenvalue", ylim=c(0,3.5),
         main = "Gráfico de sedimentación - Valores propios",
         addlabels = TRUE, xlab = "Número de componentes", 
         ylab="Valores propios")

fviz_eig(componentestotal, geom="line", ylim=c(0,50),
         main = "Gráfico de sedimentación - Varianza explicada",
         addlabels = TRUE, xlab = "Número de componentes", 
         ylab="Porcentaje de varianza explicado")

#Círculo de correlaciones para la solución factorial
#Primer plano factorial
fviz_pca_var(componentestotal, labelsize=3, repel=TRUE)+
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 7.5))+
  ggtitle("Círculo de correlaciones")

#Segundo plano factorial
fviz_pca_var(componentestotal, axes=c(3,4),labelsize=3, repel=TRUE)+
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 7.5))+
  ggtitle("Círculo de correlaciones")

#Contribuciones a los ejes factoriales
fviz_contrib(componentestotal,choice="var", axes=1)
fviz_contrib(componentestotal,choice="var", axes=2)

#Ver los valores de las contribuciones
#Paquete FactoMineR
# install.packages("FactoMineR")
# library(FactoMineR)
carteracomp <- PCA(carteraz, graph=FALSE) #ncp=5 por default
#por variable
a <- carteracomp$var$contrib
sort(a[,1], decreasing = TRUE)

#por individuo
carteracomp$ind$contrib

#Limitando el total de componentes
componentes <- prcomp(carteraz, center=TRUE, scale.=TRUE, rank.=2)
print(componentes)

#Gráfico de cargas factoriales
cargasdf <- as.data.frame(componentes$rotation)
cargasdf$medicion <- rownames(cargasdf)
cargasheat <- reshape2::melt(cargasdf)
colnames(cargasheat) <- c("variable","componente","carga")
cargasheat %>% 
  ggplot(aes(x=componente,y=variable,fill=carga, 
             label=sprintf("%0.2f", round(carga, digits=2))))+
  geom_tile()+
  scale_fill_distiller(palette="RdBu")+
  geom_text()


#Rotación de ejes para facilitar la interpretación
componentes2 <- principal(carteraz,nfactors=2,rotate="varimax")
# rotaciones disponibles: "none", "varimax", "quartimax", "promax", 
# "oblimin", "simplimax", and "cluster"
componentes2$communality
componentes2$loadings

#las comunalidades se obtienen como la suma de los cuadrados
#de las respectivas cargas de la variable en la solución factorial

sum(componentes2$loadings[1,]^2)

#Graficando de nuevo las cargas (esta vez rotadas)

componente1 <- componentes2$loadings[,1]
componente2 <- componentes2$loadings[,2]

cargasdfr <- as.data.frame(cbind(componente1,componente2))
cargasdfr$medicion <- rownames(cargasdfr)
cargasheatr <- reshape2::melt(cargasdfr)
colnames(cargasheatr) <- c("variable","componente","carga")
cargasheatr %>% 
  ggplot(aes(x=componente,y=variable,fill=carga, 
             label=sprintf("%0.2f", round(carga, digits=2))))+
  geom_tile()+
  scale_fill_distiller(palette="RdBu")+
  geom_text()
scale(carteraz)[1,]
#Graficar puntos sobre planos factoriales
#guardo las puntuaciones factoriales
pc <- componentes$x
#uno las dos columnas a la base inicial
carterapc <- cbind(cartera,pc)
carterapc$retrasos <- as.factor(carterapc$retrasos)
#dos gráficos
plot(carterapc$PC1,carterapc$PC2)

ggplot(carterapc,aes(x=PC1,y=PC2,color=retrasos))+
  geom_point()+
  ggtitle("Puntuaciones factoriales")

## Solución Rotada

#guardo las puntuaciones factoriales
pca <- componentes2$scores
#uno las dos columnas a la base inicial
carterapca <- cbind(cartera,pca)
carterapca <- carterapca %>% 
  mutate(retrasos = as.factor(retrasos))
 
# carterapca$retrasos<-as.factor(carterapca$retrasos)

carterapca %>% 
  ggplot(aes(x=RC1,y=RC2,color=retrasos))+
  geom_point()+
  ggtitle("Puntuaciones factoriales - solución rotada")


# Ejercicio 2 - Papas -----------------------------------------------------


fritasfp <- read_excel("fritas1.xlsx")
# Marca como nombre de fila
fritasf <- fritasfp %>% 
  remove_rownames %>% 
  column_to_rownames(var="Marca")

fritasf %>% glimpse()

fritasf %>% dim()

# Análisis de correlaciones
corrmatrix <- cor(fritasf[,1:38])
corrplot(corrmatrix, tl.cex=0.5, type="upper")

# Mapa de calor en distintos colores
heatmap(corrmatrix)
heatmap(corrmatrix, col=terrain.colors(12))
heatmap(corrmatrix, col=cm.colors(8))

# Usando FactoMineR
frifact <- PCA(fritasf, ncp=6, quanti.sup = c(39:42), graph = FALSE)

# Las variables suplementarias se representan en la solución factorial en 
# términos de su correlación con las componentes principales.
frifact$quanti.sup
frifact$ind$coord

nat <- fritasf$naturalidad
cor(frifact$ind$coord[,1],nat)

# gráfico de codo
fviz_eig(frifact, geom="line", choice="eigenvalue", ylim=c(0,14),
         main = "Gráfico de codo - Valores propios",
         addlabels = TRUE, xlab = "Número de componentes", 
         ylab="Valores propios")

fviz_eig(frifact, geom="bar", ylim=c(0,50),
         main = "Gráfico de codo - Varianza explicada",
         addlabels = TRUE, xlab = "Número de componentes", 
         ylab="Porcentaje de varianza explicado")

fviz_eig(frifact, geom=c("bar", "line"), ylim=c(0,50),
         main = "Gráfico de codo - Varianza explicada",
         addlabels = TRUE, xlab = "Número de componentes", 
         ylab="Porcentaje de varianza explicado")

# Calcular componentes
componentes2 <- prcomp(fritasf[,1:38],center=TRUE, scale.=TRUE)
summary(componentes2)

#contribución de variables e individuos
fviz_contrib(frifact,choice="var", axes=1,
             title = "Contribución de variables a la PC1")
fviz_contrib(frifact,choice="ind", 
             title = "Contribución de individuos a la PC1")

# Visualización de la solución PCA
fviz_pca_var(frifact,axes=c(1,2), geom =c("point","text"),repel=TRUE,
             labelsize = 3)
fviz_pca_var(frifact,axes=c(3,4), geom =c("point","text"),repel=TRUE,
             labelsize = 3)

# adicional sobre los factores
# MFA (PCA with additional grouping variables)
frifact2<-MFA(fritasf,c(11,4,10,12,5),
              name.group=c("Visual","Tactil","Empaque/sabor","Conceptos","Premiumness"),
              ncp=6,num.group.sup = c(5), graph=FALSE)
fviz_mfa_var(frifact2,axes=c(1,2), geom =c("point","text"),
             repel = TRUE, labelsize=3)
fviz_mfa_var(frifact2,axes=c(3,4), geom =c("point","text"),
             repel = TRUE, labelsize=3)

# Gráficos de individuos sobre plano factorial
fviz_pca_ind(frifact, ggtheme=theme_classic(), repel=TRUE,
             labelsize = 3)


# Clustering adicional sobre los individuos
friclust<-HCPC(frifact)
res.hcpc<-friclust
fviz_dend(friclust)

fviz_dend(friclust, rect=TRUE,rect_fill = TRUE,k_colors=c("orange","blue","red"), cex=0.5)

fviz_cluster(res.hcpc, repel = TRUE, palette=c("red","blue","salmon"))

