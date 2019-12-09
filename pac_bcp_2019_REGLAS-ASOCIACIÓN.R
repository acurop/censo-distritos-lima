# Instalación de Paquetes

install.packages(c("arules","arulesViz","TSP"),
                 dependencies = c("Depends"))

#########################################################
#  Ejemplo: plan de contrataciones bcp 2019             #
#########################################################

##LOS TIPOS DE PROCEDIMIENTOS SON LOS SIGUIENTES#
#TIPO DE PROCESO
#LP   - Licitación Pública
#CP - Concurso Público
#AS - Adjudicación Simplificada
#CPC - Compras por Catálogo - Convenio Marco
#CD - Contratación Directa
#SIE - Subasta Inversa Electrónica
#CDP - Comparación de Precios
#SCI - Selección de Consultores Individuales
#AMC - Adjudicación de Menor Cuantía
##
#_______________________________________________________
# Paso 1: Obtener y procesar la data

# Usando librería arules
library(arules)
library(arulesViz)
pac_bcp=read.transactions("https://raw.githubusercontent.com/acurop/censo-distritos-lima/master/plan_anual_contrataciones_bcp.csv", sep =";")
head(pac_bcp)

summary(pac_bcp)

# Mostrar las transacciones
labels(pac_bcp)

# Mostrar un subconjunto de transacciones (p. ej. las 6 primeras)
inspect(pac_bcp[1:6])

# Mostrar el soporte (proporción de transacciones) de un item (p. ej. de los cuatro primeros)
itemFrequency(pac_bcp[, 1:4])

# Visualizar el soporte de los items (p. ej. de aquellos items con una proporción mayor a 0.10)
itemFrequencyPlot(pac_bcp, support = 0.1)

# Visualizar el soporte de los items (p. ej.de los 15 ítems con mayor soporte)
itemFrequencyPlot(pac_bcp, topN = 15)

# Visualizar la matriz de transacciones (p. ej. para las 10 primeras transacciones)
image(pac_bcp[1:10])

# Visualizar la matriz de transacciones (p. ej. seleccionar al azar 70 transacciones)
image(sample(pac_bcp, 70))
#----------------------------------------------------------------------------------
# Interpretaciones :
# ---------------------------------------------------------------------------------
# Los puntos deberían verse dispersos con un patrón aleatorio.
# Buscar patrones no aleatorios:
# - Si un ítem aparece en todas las transacciones podría tratarse de información que no 
#   corresponde a un item comprado.
# - Si se ordenan los items por algún criterio, por ejemplo por el tipo de procedimiento, podrían
#   detectarse algún comportamiento estacional (BIENES O SERVICIOS)
#-----------------------------------------------------------------------------------

#_______________________________________________________
# Paso 2: Entrenar el modelo con los datos
#________________________
apriori(pac_bcp)
#----------------------------------------------------------------------------------
# Comentario :
# ---------------------------------------------------------------------------------
# Recordar que por defecto un soporte = 0.1 es usado para generar una regla, es decir
# que al menos un item debe aparecer en 0.1 * 9385 = 938.5 transacciones. Dado que solo
# ocho item tienen esta frecuencia, es bastante predecible que no se encuentre ninguna 
# regla de asociacion.


pac_bcp_rules <- apriori(pac_bcp, parameter = list(support =0.006,
                                                      confidence = 0.25, minlen = 2))

pac_bcp_rules
summary(pac_bcp_rules)
#_______________________________________________________
# Paso 3: Evaluar el modelo
#_______________________________________________________

summary(pac_bcp_rules)
#----------------------------------------------------------------------------------
# Mostrar las 5 primeras reglas de asociacion
inspect(pac_bcp_rules[1:5])

#_______________________________________________________
# Paso 4: Mejorar la performance del modelo
#_______________________________________________________


# Mostrar las 5 reglas con mayor lift
inspect(sort(pac_bcp_rules, by = "lift")[1:5])
# Subconjuntos de reglas
subrules <- subset(pac_bcp_rules, items %in% "tipo_proc")
inspect(subrules)
#----------------------------------------------------------------------------------
# Uso de subset() :
# ---------------------------------------------------------------------------------
# - La palabra clave items empareja un item que aparezca en alguna regla. Es posible delimitar
#   que esta ocurra solo a la izquierda o derecha usando lhs y rhs.
# - El operador %in% significa que al menos uno de los items debe ser encontrado, de la lista de
#   items definidos.  Si se desea encontrar reglas con tipo de procedimiento,debería escribirse
#   %in%c("bienes ", "servicios”).
#-----------------------------------------------------------------------------------
# Exportar las reglas obtenidas
write(pac_bcp_rules, file = "pac_bcp_rules.csv",
      sep = ";", quote = TRUE, row.names = FALSE)

# Convertir reglas en dataframe
pac_bcp_rules_df <- as(pac_bcp_rules, "data.frame")
head(pac_bcp_rules_df)


# Visualización
rules <- apriori(pac_bcp, parameter =
                   list(supp = 0.01, conf = 0.5, target = "rules")) 

install.packages("arulesViz")
install.packages("foreach")
library(arulesViz)
plot(rules)

subrules <- head(sort(rules, by="lift"), 10)

plot(subrules,method="graph",control=list(alpha=1))

plot(rules,method="matrix",measure="support")

plot(rules,method="matrix3D",measure="confidence")





