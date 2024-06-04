## Revisión de que los paquetes estén instalados
if (!require('readxl')) install.packages('readxl')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('dendextend')) install.packages('dendextend')
if (!require('psych')) install.packages('psych')

## Leer los datos que deben estar en la ruta que se genera al correr el
## siguiente código
getwd()

mundo <- read_excel("mundo2014ampliado.xlsx")
mundo %>% glimpse()

## Solo los datos que se van a usar
#extraer solo América

#usando dplyr
america <- mundo %>% filter(america==1)

# usando base
# america<-subset(mundo,mundo$america==1)

#extraer los nombres de los países y 
#guardarlos como nombres de las filas del dataframe
america <- america %>% 
  remove_rownames %>% 
  column_to_rownames(var = "Country")

# usando base
# rownames(america)<-america$Country
# america

#extraer las variables que necesitamos
# usando dplyr
americafin <- america %>% 
  select(c(2,3,6:8))

# usando base
# americafin <- america[,c(2,3,6:8)]

#estandarizar
americafins <- americafin %>% scale()
#calcular la matriz de distancias
americadist <- dist(americafins,method="euclidean")

## Modelado

#cluster complete linkage
#crear el cluster jerárquico. Por defecto es complete linkage
americacomplete <- hclust(americadist)
americacomplete$height

#algunos arreglos gráficos
grafdend <- as.dendrogram(americacomplete)
grafdend <- set(grafdend,"labels_cex",0.5)
#ver el resultado
plot(grafdend, main="Complete")

# Dendograma con ggplot2
install.packages("ggdendro")
library(ggdendro)
ggdendrogram(americacomplete, rotate = FALSE, size = 2)+
  ggtitle("Complete")+
  theme(axis.text.x = element_text(size=7))

#cluster single linkage: proceso similar
americasingle <- hclust(americadist,method="single")
grafdends <- as.dendrogram(americasingle)
grafdends <- set(grafdends,"labels_cex",0.5)
plot(grafdends, main="Single")

## Ver variables de influencia
# mapa de calor
heatmap(americafins,hclustfun=hclust,cexCol = 0.7, cexRow = 0.7)

# cortar el dendograma a un nivel dado y guardar el grupo de pertenencia
americajcluster <- cutree(americacomplete,k=6)
#unir a la base de datos para análisis subsiguiente
americafins2 <- as.data.frame(cbind(americafins,americajcluster))
#dejar la variable de grupo como factor
americafins2 <- 
  americafins2 %>% 
  mutate(americajcluster = as.factor(americajcluster))
#graficar
ggplot(aes(x=Fertility_rate, y=Pib_growth,col=americajcluster), 
       data=americafins2)+
  geom_point(size=3)+
  geom_text(label=rownames(americafins2),
            size=3, col="black", nudge_y = -0.1)+
  labs(color="Grupo")+
  theme_minimal()

#-------------------------------------------------------------------------------------------------------------
# Exploratorio por clústeres
#-------------------------------------------------------------------------------------------------------------

#Boxplot segmentado por clúster

americafins2 %>% ggplot(aes(x=americajcluster, y=Pib_growth))+
  geom_boxplot()+
  ggtitle("Crecimiento del PIB por clúster")+
  labs(x="Grupo", y="Pib growth")+
  theme_minimal()

# Paquete básico
# boxplot(Pib_growth~americajcluster, data=americafins2)

ggplot(americafins2, aes(x=americajcluster,y=Fertility_rate))+
  geom_boxplot(outlier.shape = 1)+
  ggtitle("Tasa de fertilidad por clúster")+
  labs(x="Grupo", y="Fertility rate")+
  theme_minimal()

ggplot(americafins2, aes(x=americajcluster,y=Life_expectancy))+
  geom_boxplot(outlier.shape = 1)+
  ggtitle("Expectativa de vida por clúster")+
  labs(x="Grupo", y="Life expectancy")+
  theme_minimal()

ggplot(americafins2, aes(x=americajcluster,y=tiempo_escolar))+
  geom_boxplot(outlier.shape = 1)+
  ggtitle("Escolaridad por clúster")+
  labs(x="Grupo", y="Tiempo escolar")+
  theme_minimal()

ggplot(americafins2, aes(x=americajcluster,y=`Pib/capita`))+
  geom_boxplot(outlier.shape = 1)+
  ggtitle("PIB per cápita por clúster")+
  labs(x="Grupo", y="Pib/cápita")+
  theme_minimal()

#Resumen descriptivo por clúster

describeBy(americafins2,americafins2$americajcluster)

#Gráfico de perfiles
#En jerárquico resulta más útil un heatmap

if (!require('GGally')) install.packages('GGally')

centros <- americafins2 %>% 
  group_by(americajcluster) %>% 
  summarize_all(mean)
centros <- as.data.frame(centros)
str(centros)

ggparcoord(centros, columns = 2:6, groupColumn = 1, scale = "globalminmax")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90))+
  ggtitle("Gráfico de centros")

#----------------------------------------------------------------------------------------
# Calidad del clustering
#----------------------------------------------------------------------------------------

#Distancia implícita en el dendograma
cophenetic(americacomplete) 
#correlación entre la distancia original y la del dendograma
#a mayor valor (>0.75), mejor es el clustering
cor(americadist,cophenetic(americacomplete)) 


#----------------------------------------------------------------------------------------
# CLUSTER PACKAGE 
#----------------------------------------------------------------------------------------

if (!require('cluster')) install.packages("cluster")

hcagnes <- agnes(americafin, stand=TRUE,method="complete", metric = "euclidean")
grafagnes<-as.dendrogram(hcagnes)
grafagnes1<-set(grafagnes,"labels_cex",0.5)
plot(grafagnes1, main="complete")

agnescluster <- cutree(hcagnes,k=6)
americagnes<-as.data.frame(cbind(americafin,agnescluster))
americagnes$agnescluster<-as.factor(americagnes$agnescluster)
ggplot(aes(x=Fertility_rate, y=Pib_growth,col=agnescluster), 
       data=americagnes)+
  geom_point(size=3)+
  geom_text(label=rownames(americafin),
            size=3, col="black")
