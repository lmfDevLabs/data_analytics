if (!require('psych')) install.packages('psych') #Elegant way to check for missing packages and install them
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('dendextend')) install.packages('dendextend')
if (!require('dbscan')) install.packages('dbscan')
if (!require('readxl')) install.packages('readxl')

# Lectura de los datos que deben estar en la ruta que genera el siguiente código
getwd()

explicluster <- as.data.frame(readRDS("clusterempresas.rds")) #rds: relational database 
# gráfico
ggplot(explicluster, aes(x=loggross, y=lognet))+
  geom_point(size=1)+
  ggtitle("Diagrama de dispersión")+
  labs(x="loggross", y="lognet")

explicluster %>% glimpse()

## Descriptivos

#basarnos en la base de datos ya nombrada
attach(explicluster)

#histogramas y descriptivos
#ganancia bruta
explicluster %>% ggplot(aes(x=loggross))+
  geom_histogram(color="black", fill="gray")+
  ggtitle("Histograma de loggross")+
  labs(x="Loggross", y="")+
  theme_minimal()

#ganancia neta
ggplot(explicluster, aes(x=lognet))+
  geom_histogram(color="black", fill="gray")+
  ggtitle("Histograma de lognet")+
  labs(x="Lognet", y="")+
  theme_minimal()

psych::describe(explicluster)

## Aplicar el algoritmo k medias

#hacer el cluster
set.seed(999)
empresaskmcluster <- kmeans(explicluster, centers=4, nstart=10)
#ver las iteraciones
empresaskmcluster$iter

#volver los grupos categóricos para graficarlos
#Creo una variable grupfin con el vector de clúster y la convierto en categórica
explicluster <- explicluster %>% 
  mutate(grupfin = as.factor(empresaskmcluster$cluster))

#graficar con color para los clústeres
gr1 <- explicluster %>% 
  ggplot(aes(loggross,lognet,color=grupfin))+
           geom_point()

#agregar los centroides
centroides <- as.data.frame(empresaskmcluster$centers)

gr1 + geom_point(aes(centroides[,1], centroides[,2]), 
                 color="black", size=2, data=centroides)+
  labs(color="Grupo")


#le quito la asignación actual de cluster
explicluster2 <- explicluster[,1:2]
#creo una matriz vacía para la nueva asignación por iteración
asig <- matrix(nrow=nrow(explicluster2),ncol=3)
#guardo asignación y centros de cluster por iteración
for (i in 1:3) {set.seed(999)
  asig[,i] <- as.factor(kmeans(explicluster2,centers=4, nstart=10, iter.max = i)$cluster)
  assign(paste("centro", i, sep=""),kmeans(explicluster2,centers=4, nstart=10, iter.max = i)$centers)}
#creo base con las tres asignaciones
explicluster2[,3:5] <- as.factor(asig[,1:3])
#grafico los tres
#iteración 1
gr1 <- ggplot(aes(loggross,lognet,color=explicluster2[,3]), data=explicluster2)+geom_point()
centro1 <- as.data.frame(centro1)
gr1+geom_point(aes(centro1[,1], centro1[,2]), color="black", size=2, data=centro1)+
  labs(color="Grupo\nIteración 1")
centro1
centro2
#iteración 2
gr2 <- ggplot(aes(loggross,lognet,color=explicluster2[,4]), data=explicluster2)+geom_point()
centro2 <- as.data.frame(centro2)
gr2+geom_point(aes(centro2[,1], centro2[,2]), color="black", size=2, data=centro2)+
  labs(color="Grupo\nIteración 2")

#iteración 3
gr3 <- ggplot(aes(loggross,lognet,color=explicluster2[,5]), data=explicluster2)+geom_point()
centro3 <- as.data.frame(centro3)
gr3+geom_point(aes(centro3[,1], centro3[,2]), color="black", size=2, data=centro3)+
  labs(color="Grupo\nIteración 3")



#-------------------------------------------------------------------------------------------------------------
# COMBINACIÓN DE K-MEDIAS Y CLÚSTER JERÁRQUICO
#-------------------------------------------------------------------------------------------------------------

#hacer el cluster
set.seed(999)
empresaskmcluster_20 <- kmeans(explicluster,centers=20, nstart=10,iter.max = 20)

#volver los grupos categóricos para graficarlos
explicluster <- explicluster %>% 
  mutate(grupfin = as.factor(empresaskmcluster_20$cluster))

#graficar con color para los clústeres
gr1<-ggplot(aes(loggross,lognet,color=grupfin), data=explicluster)+geom_point()+
  guides(color=guide_legend(ncol=2))+
  labs(color="Grupo")
#agregar los centroides
centroides <- as.data.frame(empresaskmcluster_20$centers)
centroides$grupfin <- as.character(rownames(centroides))
gr1+geom_point(aes(centroides[,1], centroides[,2]), color="black", 
               size=1.5, data=centroides)+
  geom_text(aes(label=grupfin),hjust=1, vjust=1, data=centroides, color="black")

#calcular la matriz de distancias
empresas_dist <- dist(centroides,method="euclidean")

#cluster complete linkage
#crear el cluster jerárquico. Por defecto es complete linkage
empresas_complete <- hclust(empresas_dist)

#algunos arreglos gráficos
grafdend <- as.dendrogram(empresas_complete)
grafdend <- set(grafdend,"labels_cex",0.5)
#ver el resultado
plot(grafdend,horiz=FALSE, main="Complete")

##ahora tomemos una solución, p.e, 4 grupos
jerarquico <- cutree(empresas_complete,k=4)
jerarq2 <- as.data.frame(cbind(jerarquico,seq(1:20)))
jerarq2$v2 <- as.factor(jerarq2$V2)                       

explicluster3<-left_join(explicluster,jerarq2,by=c("grupfin"="v2"))
explicluster3$jerarquico<-as.factor(explicluster3$jerarquico)
ggplot(data= explicluster3, aes(x=loggross,y=lognet, color=jerarquico))+ 
  geom_point()+
  labs(color="Grupo")


## Single
empresas_single <- hclust(empresas_dist, method = "single")
#algunos arreglos gráficos
grafdend_s<-as.dendrogram(empresas_single)
grafdend_s<-set(grafdend_s,"labels_cex",0.5)
#ver el resultado
plot(grafdend_s,horiz=FALSE, main="Single")

##ahora tomemos una solución, p.e, 4 grupos
jerarquico<-cutree(empresas_single,k=4)
jerarq2<-as.data.frame(cbind(jerarquico,seq(1:20)))
jerarq2$v2<-as.factor(jerarq2$V2)                       

explicluster3<-left_join(explicluster,jerarq2,by=c("grupfin"="v2"))
explicluster3$jerarquico<-as.factor(explicluster3$jerarquico)
ggplot(data= explicluster3, aes(x=loggross,y=lognet, color=jerarquico))+ 
  geom_point()+
  labs(color="Grupo")


## Average
empresas_av <- hclust(empresas_dist, method = "average")
#algunos arreglos gráficos
grafdend_av<-as.dendrogram(empresas_av)
grafdend_av<-set(grafdend_av,"labels_cex",0.5)
#ver el resultado
plot(grafdend_av,horiz=FALSE, main="Average")

##ahora tomemos una solución, p.e, 4 grupos
jerarquico<-cutree(empresas_av,k=4)
jerarq2<-as.data.frame(cbind(jerarquico,seq(1:20)))
jerarq2$v2<-as.factor(jerarq2$V2)                       

explicluster3<-left_join(explicluster,jerarq2,by=c("grupfin"="v2"))
explicluster3$jerarquico<-as.factor(explicluster3$jerarquico)
ggplot(data= explicluster3, aes(x=loggross,y=lognet, color=jerarquico))+ 
  geom_point()+
  labs(color="Grupo")

#-------------------------------------------------------------------------------------------------------------
# ALGORITMO DB-SCAN
#-------------------------------------------------------------------------------------------------------------

expliclusterz <- scale(explicluster[,1:2])

kNNdistplot(expliclusterz,k=50)
dbout<-dbscan(expliclusterz,eps=0.25)

#Tabla de tamaños de los grupos
table(dbout$cluster)

mirar<-cbind(expliclusterz,as.data.frame(dbout$cluster))
colnames(mirar)<-c("loggross","lognet","grupo")
mirar$grupo<-as.factor(mirar$grupo)
##uno a la base de datos y grafico
ggplot(data= mirar, aes(x=loggross,y=lognet, color=grupo))+ 
  geom_point()+
  labs(color="Grupo\nDB-Scan")
