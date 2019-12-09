reporte=read.csv("https://raw.githubusercontent.com/acurop/censo-distritos-lima/master/emapa.csv", sep =";")
head(reporte)
reporte = reporte[,1:7]

head(reporte)
X=reporte[-8,-1]
Z=scale(X[,1:6])
row.names(Z)=as.character(reporte[-7,1])
Z
cj=hclust(dist(Z)^2, method = "average") 
cj$merge 
cj$height
print(cj) 
plot(cj,main="Dendograma",labels=row.names(Z),hang=-1) 
