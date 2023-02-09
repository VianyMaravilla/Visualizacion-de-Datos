#Proyecto 5
#Maravilla Perez Vianey
#Mondolla Cervantes Erin
#5AM1

#Desarrollar un programa en R que permita calcular la similitud del coseno a partir del conjunto de datos:
#Abstract COVID Papers.csv

#Instrucciones: 
#1.Analizar el conjunto de datos que contiene 3 atributos: "title", "abstract" y "url".
#2.Revisar el corpus de datos y los metadatos directamente de la p?gina: https://www.kaggle.com/datasets/anandhuh/covid-abstracts
#3.Seleccionar 100 abstracts y aplicarles la similitud del coseno. Cada equipo debe seleccionar diferentes
#instancias para que no sean iguales. El dataset est? compuesto por 10,000 instancias.
#4.Posteriormente, rankear los abstracts por grado de similitud y visualizarlos por el t?tulo con una representaci?n gr?fica (opcional tipo de gr?fico).



###########################################################################################################################################

#Fragmentos analizados -> Desde 1,301 al 1,400
#leemos el archivo csv
abstract <- read.csv("C:/Users/viane/Music/ESCOM/Análisis y visualización de datos/Prácticas/Proyecto 05/covid_abstracts.csv")

###########################################################################################################################################

#seleccionar los abstracts desde  8300 hasta 8400
library(lsa)
library(proxy)
library(readr)
library(tm)
#install.packages(pheatmap)
library(pheatmap)
abstract <- abstract[1301:1400,]


###########################################################################################################################################

#obtener el numero de abtracts
nrow(abstract)
text = abstract[,2]
corpus <- VCorpus(VectorSource(text))
tdm <- TermDocumentMatrix(corpus, 
                          control = list(wordLengths = c(1, Inf)))
occurrence <- apply(X = tdm, 
                    MARGIN = 1, 
                    FUN = function(x) sum(x > 0) / ncol(tdm))
tdm_mat <- as.matrix(tdm[names(occurrence)[occurrence >=0.5],])
lsaSpace <- lsa(tdm_mat)

###########################################################################################################################################

#lsaMatrix ahora es una matriz k x (num doc), en un espacio LSA k-dimensional
lsaMatrix <- diag(lsaSpace$sk) %*% t(lsaSpace$dk)

###########################################################################################################################################

# Use la función `coseno` en el paquete `lsa` para obtener la matriz de similitudes de coseno
distMatrix <- cosine(lsaMatrix)
resultado_final <- round(distMatrix, 3)



pheatmap(resultado_final, cluster_rows = FALSE, cluster_cols = FALSE)

###########################################################################################################################################

# Se imprimen el top 10 de documentos más similares in distMatrix
for (i in 1:10) {
  print(abstract[order(distMatrix[i, ], decreasing = TRUE)[2], 1])
}
