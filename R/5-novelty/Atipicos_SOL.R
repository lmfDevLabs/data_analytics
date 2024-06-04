
library(dbscan)
library(solitude)
library(readxl)
library(Rcpp)

# LECTURA DE DATOS

getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
datos <- read_excel("Datos_bancos - Copy.xlsx", sheet=1)


# Enfoque boxplot


datos_num <- datos[,2:3]

mi_primer_boxplot <- boxplot(datos_num$Tasa_promedio)


mi_primer_boxplot$out


length(mi_primer_boxplot$out)/nrow(datos_num)


datos$atipicos <- 1
datos[datos_num$Tasa_promedio %in% mi_primer_boxplot$out,4] <- -1


plot(datos$Tasa_promedio, datos$Monto_desembolsado, col= as.factor(datos$atipicos))



# Z - score


z_score <- (datos_num$Tasa_promedio - mean(datos_num$Tasa_promedio))/sd(datos_num$Tasa_promedio)

datos[abs(z_score)>=3,]

z_score_mod <- (datos_num$Tasa_promedio - mean(datos_num$Tasa_promedio))/mad(datos_num$Tasa_promedio)

datos[abs(z_score_mod)>=3,]


plot(datos$Tasa_promedio, datos$Monto_desembolsado)

# DBScan

datos_esc <- scale(datos[,2:3])

kNNdistplot(datos_esc, k=3)


db <- dbscan(datos_esc,eps = 0.7)

db$cluster

plot(datos$Tasa_promedio, datos$Monto_desembolsado, col = as.factor(db$cluster))

# Isolation Forest


isoForest <- isolationForest$new(sample_size = as.integer(nrow(datos)/2),
                                 num_trees=1000)

isoForest$fit(datos_esc)

pred <- isoForest$predict(datos_esc)

datos$atipicos_iso <- 1
datos[pred$anomaly_score>=0.6,5]<- -1

plot(datos$Tasa_promedio, datos$Monto_desembolsado, col = as.factor(datos$atipicos_iso))





