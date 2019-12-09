reporte=read.csv("https://raw.githubusercontent.com/acurop/censo-distritos-lima/master/emapa.csv", sep =";")

#####
#LOS DATOS DE ESTUDIO SON DE EMAPACA CAÑETE
#DISTRITOS
#CANTIDAD DE POBLACIÓN 
#NÚMERO DE CONEXIONES ESTABLECIDAD
#TOTAL DE POBLACIÓN ABASTECIDA
#PORCENTAJE DE COBERTURA


#######################
#Se desea encontrar un agrupamiento de lOS distritos de cañete que refleje posibles
#igualdades entre subconjuntos de ellas mismas. 
#######################


head(reporte)
reporte = reporte[,1:7]

head(reporte)
####ANALISIS CON VARIABLES ESTANDARIZADAS###
X=reporte[-7,-1]

##
#Usando la función scale para estandarizar las variables, dada la diversidad en la escala usada##
#en cada una de ellas. 
Z=scale(X[,1:6])
row.names(Z)=as.character(reporte[-7,1])
Z
cj=hclust(dist(Z)^2, method = "average") 

##TABLA DE AGRUPAMIENTO##
cj$merge 
cj$height #valor de medida proximidad a la que se van formando los cluster 
print(cj) 
plot(cj,main="Dendograma",labels=row.names(Z),hang=-1) 
############
#Según el dendrograma anterior, si decidimos establecer 2 cluster, estarán formados por
#los siguientes casos:
#Cluster: San Vicente, Imperial y Mala
#Cluster: Lunahuana, Quilmana, San luis, Santa cruz, Cerro azul, San antonio

###ANÁLISIS CLUSTER CON EL MÉTODO KMEANS

##Partiremos de un agrupamiento formado por 3 cluster##
kmeans(Z,3)

#Usaremos ahora las dos primeras componentes principales de las variables del data
#frame Z para construir los 3 grupos mediante el método kmeans.
#Previo análisis de componentes principales. Resume las 8 variables en las 2 que
#capturan la máxima variabilidad del total##
acp=princomp(Z)


#Tomamos las puntuaciones en las componentes primeras para cada distrito de cañete#
comp=predict(acp)[,1:3] 

#Análisis cluster con las 2 componentes principales#
km2=kmeans(comp,3) 

#Representación gráfica de los distritos de cañete según las puntuaciones en las componentes
#y el grupo al que se han asignado. Representación de los centroides (medias de los grupos en las componentes)# 

plot(comp,col=km2$cluster) #cada cluster de un color 
points(km2$centers, col = 1:4, pch = 8, cex=2) #medias de los cluster en las componentes
text(comp[,1],comp[,2],labels=rownames(Z),col=km2$cluster) #etiquetas de nombres Comunidades
