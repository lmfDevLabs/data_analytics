

#Exportar datos
bancos <- read_excel("C:/Users/aulasingenieria/Downloads/Datos_bancos.xlsx")

#Convertir a DataFrame
df_bancos <- as.data.frame(bancos)
datos_originales <- as.data.frame(bancos) 
#Analisis Univariado
hist(df_bancos$Tasa_promedio)
boxplot(df_bancos$Tasa_promedio)
hist(df_bancos$Monto_desembolsado)
boxplot(df_bancos$Monto_desembolsado)
#Analisis Multivariado
plot(df_bancos$Tasa_promedio, df_bancos$Monto_desembolsado)

#Escalar magnitudes
df_bancos$Tasa_promedio <- scale(df_bancos$Tasa_promedio)
df_bancos$Monto_desembolsado <- scale(df_bancos$Monto_desembolsado)

plot(df_bancos$Tasa_promedio, df_bancos$Monto_desembolsado)

#Calcular K con grafico de codo
wss <-c()
for (i in 1:15) wss[i] <- sum(kmeans(df_bancos[,2:3],
                                     centers=i, nstart=10)$withinss)
# Gr?fico de codo
# Con ggplot
sumas <- as.data.frame(cbind(wss, k = seq(1,15, by=1)))

plot(1:15, wss, type="b", xlab="N?mero de Cl?steres",
     ylab="Suma de cuadrados within")


#Calcular K con AWS

library("cluster")
library("fpc")
set.seed(2) #Para evitar aleatoriedad en los resultados
clustering.asw <- kmeansruns(df_bancos[,2:3],
                             krange=2:15,criterion="asw",
                             iter.max=100,runs= 100,critout=TRUE)
clustering.asw$bestk

#Calcular K con GSCAR

set.seed(21)
gscar<-clusGap(df_bancos[,2:3],FUN=kmeans,K.max=15,B=60, iter.max=20)
gscar$Tab[,"gap"]
gscar$Tab[,"gap"]-gscar$Tab[,"SE.sim"]


#Realizar cluster con k elegido

clusters <- kmeans(df_bancos[,2:3],centers=3, nstart=10)
grupos <- clusters$cluster
df <- as.data.frame(cbind(datos_originales,grupos))

#Ver resultados

plot(df$Tasa_promedio, df$Monto_desembolsado, col=df$grupos)
text(df$Tasa_promedio, df$Monto_desembolsado,
     labels = df$Banco,
     cex = 0.6, pos = 4)

#Gr?fico de calor de centros
centrosg <- as.data.frame(clusters$centers)
centrosg$grupo <- as.factor(rownames(centrosg))
centrosheat <- reshape2::melt(centrosg)
colnames(centrosheat) <- c("grupo","variable","centroide")
centrosheat %>% 
  ggplot(aes(x=grupo,y=variable,fill=centroide, label=sprintf("%0.2f", round(centroide, digits=2))))+
  geom_tile()+
  scale_fill_distiller(palette="RdBu")+
  geom_text()
