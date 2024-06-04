#################################################################################

########################## EJERCICIO PRACTICO PCA ###############################

#################################################################################

if (!require('corrplot')) install.packages('corrplot')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('psych')) install.packages('psych')
if (!require('RColorBrewer')) install.packages('RColorBrewer')
if (!require('factoextra')) install.packages('factoextra')
if (!require('reshape2')) install.packages('reshape2')
if (!require('FactoMineR')) install.packages('FactoMineR')
if (!require('readxl')) install.packages('readxl')

# El objetivo en este ejercicio es reducir el número de variables económico-financieras 
# que describen a un grupo de empresas dedicadas al transporte de mercancías 
# por carretera, conservando el máximo de información posible.



# Lectura de los datos que deben estar en la ruta que genera el siguiente código
getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load('datos_PCA.RData')

#Fuente: Universidad de Murcia

#apalanc: apalancamiento (%)
#capital: capital social (miles de euros)
#ebitda.ventas: ebitda sobre ventas (%)
#empleados: plantilla de la empresa (personas)
#fondos: fondos propios (miles de euros)
#ingresos: ingresos de explotación (miles de euros)
#reco: rentabilidad económica (%)
#rfin: rentabilidad financiera (%)
#res.ventas: resultado del ejercicio sobre ventas (%)

###### Siga lo siguientes pasos para lograr el objetivo:

###### 1. Explore los datos

str(datos)

summary(datos)

boxplot(datos$ingresos)

hist(datos$ingresos)

###### 2. Evaluar correlaciones

correlaciones <- round(cor(datos),4)

corrplot(correlaciones, type="upper", tl.cex = 0.7,col=brewer.pal(n=8, name="PuOr"))

Test.Bartlett <- cortest.bartlett(correlaciones,n=nrow(datos))
Test.Bartlett

#Test KMO
KMO(datos)


###### 3. Calcule los componentes principales y analice

componentestotal <- prcomp(datos, center=TRUE, scale.=TRUE)

summary(componentestotal)

componentestotal$rotation

componentestotal$scale

mean(datos$apalanc)

datos_nuevos<-componentestotal$x


cor_nueva<-cor(datos_nuevos)

corrplot(cor_nueva, type="upper", tl.cex = 0.7,col=brewer.pal(n=8, name="PuOr"))


###### 4. Determine el numero de componentes principales a retener


fviz_pca_var(componentestotal, axes=c(1,2),labelsize=3, repel=TRUE)+
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 7.5))+
  ggtitle("Círculo de correlaciones")

fviz_eig(componentestotal, geom="line", choice="eigenvalue", ylim=c(0,3.5),
         main = "Gráfico de sedimentación - Valores propios",
         addlabels = TRUE, xlab = "Número de componentes", 
         ylab="Valores propios")

fviz_contrib(componentestotal,choice="var", axes=1)

###### 5. Interprete los componentes principales que conservará, ¿a que factores latentes se pueden asociar? Describalos



componentestotal <- prcomp(datos, center=TRUE, scale.=TRUE, rank.=3)


#Gráfico de cargas factoriales
cargasdf <- as.data.frame(componentestotal$rotation)
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
componentes2 <- principal(datos,nfactors=3,rotate="varimax")

componente1 <- componentes2$loadings[,1]
componente2 <- componentes2$loadings[,2]
componente3 <- componentes2$loadings[,3]

cargasdfr <- as.data.frame(cbind(componente1,componente2, componente3))
cargasdfr$medicion <- rownames(cargasdfr)
cargasheatr <- reshape2::melt(cargasdfr)
colnames(cargasheatr) <- c("variable","componente","carga")
cargasheatr %>% 
  ggplot(aes(x=componente,y=variable,fill=carga, 
             label=sprintf("%0.2f", round(carga, digits=2))))+
  geom_tile()+
  scale_fill_distiller(palette="RdBu")+
  geom_text()






cor(datos,componentestotal$x)









