# IMPORTACI?N DATOS DIRECTA
# ==============================================================================
library(arules)
library(arulesviz)

getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

transacciones <- read.transactions(file = "ejercicio_compras.csv",
                                   format = "single",
                                   sep = ",",
                                   header = TRUE,
                                   cols = c("id_compra", "item"),
                                   rm.duplicates = TRUE)
transacciones


# Explore los datos

inspect(transacciones[1:5,])

summary(size(transacciones))

datos <- read.csv("ejercicio_compras.csv")

# Defina las reglas de asociacion. ?Como sustenta la elecci?n del soporte y cofianza umbral?

itemFrequencyPlot(transacciones, topN=15, 
                  cex.names=.7, ylim=c(0,0.6), ylab="Frecuencia de compra relativa")


# Evalue y analice las reglas creadas

rules <- apriori(transacciones,
                 parameter=list(support=0.025,confidence=0.4))

summary(rules)

#?Qu? productos se venden bien juntos? De 3 estrategias de venta

inspect(head(sort(rules,by="lift"),9))


subrules2 <- head(sort(rules, by="lift"), 9)
plot(subrules2, method="graph")

# La direcci?n quiere recomendaciones especificas para los productos "whole milk" y "soda". ?Que estrategias
# planteria para esos dos productos?


filtrdos <- subset(rules, subset= items %in% "whole milk")
inspect(head(sort(filtrdos,by="lift"),9))
