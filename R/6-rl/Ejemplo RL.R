
# PAQUETE MASS. Datos Boston. Recoje la mediana del valor de vivienda para 506 zonas con 
# la siguiente información adicional:

# crim: ratio de criminalidad per cápita de cada ciudad.
# zn: Proporción de zonas residenciales con edificaciones de más de 25.000 pies cuadrados.
# indus: proporción de zona industrializada.
# chas: Si hay río en la ciudad (= 1 si hay río; 0 no hay).
# nox: Concentración de óxidos de nitrógeno (partes per 10 millón).
# rm: promedio de habitaciones por vivienda.
# age: Proporción de viviendas ocupadas por el propietario construidas antes de 1940.
# dis: Media ponderada de la distancias a cinco centros de empleo de Boston.
# rad: Índice de accesibilidad a las autopistas radiales.
# tax: Tasa de impuesto a la propiedad en unidades de $10,000.
# ptratio: ratio de alumnos/profesor por ciudad.
# black: 1000(Bk - 0.63)^2 donde Bk es la proporción de gente de color por ciudad.
# lstat: porcentaje de población en condición de pobreza.
# medv: Valor mediano de las casas ocupadas por el dueño en unidades de $1000s.

library(MASS)
data("Boston")



