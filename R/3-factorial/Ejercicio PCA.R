#################################################################################

########################## EJERCICIO PRACTICO PCA ###############################

#################################################################################


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

###### 2. Evaluar correlaciones

###### 3. Calcule los componentes principales y analice

###### 4. Determine el numero de componentes principales a retener

###### 5. Interprete los componentes principales que conservará, ¿a que factores latentes se pueden asociar? Describalos

