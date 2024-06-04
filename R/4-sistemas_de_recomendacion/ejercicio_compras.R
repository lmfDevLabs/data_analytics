# IMPORTACIÓN DATOS DIRECTA
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


# Defina las reglas de asociacion. ¿Como sustenta la elección del soporte y cofianza umbral?


# Evalue y analice las reglas creadas


# ¿Qué productos se venden bien juntos? De 3 estrategias de venta


# La dirección quiere recomendaciones especificas para los productos "whole milk" y "soda". ¿Que estrategias
# planteria para esos dos productos?




