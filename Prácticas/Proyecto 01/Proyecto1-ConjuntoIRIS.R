# Proyecto 1
# Descripción: A partir del conjunto de datos Iris, realizar un programa
# en R que permita calcular:
# 1.- Área del sépalo y pétalo
# 2.- Calcular la media aritmética, media generalizada (considerando los valores
# vistos en clase), mediana, moda, varianza, desviación estándar y 
# valores máximos y mínimos de las áreas de los sépalos y pétalos de cada una de 
# las 3 especies de flores iris

# Vianey Maravilla Pérez 5AM1
###############################################################################################################################
# Cargar el conjunto de datos IRIS 

data("iris")

###############################################################################################################################

# Calulo del área del sépalo y pétalo

areaS <-(iris$Sepal.Length * iris$Sepal.Width)/2
areaP <-(iris$Petal.Length * iris$Petal.Width)/2

# Se meten los calculos del área a un data frame para poder visualizarlos de mejor manera

areas <- data.frame(área_sépalo_cm2 = areaS, área_pétalo_cm2 = areaP, Specie = iris$Species)

# Se manda a llamar a la función que contiene el data frame

areas

###############################################################################################################################

# Calculo de la media aritmética

# Calculo de la longitud y ancho de sépalo

LongSepal <- c(iris$Sepal.Length)
sum = 0
for (i in 1:150)
{
  sum = sum + LongSepal [i]
}
prom1 <- c(sum / length(iris$Sepal.Length))

AnSepal <- c(iris$Sepal.Width)
summ = 0
for (i in 1:150)
{
  summ = summ + AnSepal[i]
}
prom2 <- c(summ / length(iris$Sepal.Width))

# Calculo de la longitud y ancho del petalo

LongPet <- c(iris$Petal.Length)
sump = 0
for (i in 1:150)
{
  sump = sump + LongPet[i]
}
prom3 <- c(sump / length(iris$Petal.Length))

AnPet <- c(iris$Petal.Width)
summp = 0
for (i in 1:150)
{
  summp = summp + AnPet[i]
}
prom4 <- c(summp / length(iris$Petal.Width))

# Se meteran las medias dentro de un data frame para su mejor visualización

mediaSP <- data.frame(Media_LongSepal  = prom1,
                      Media_AnchoSepal = prom2,
                      Media_LongPetalo = prom3,
                      Media_AnchoPet   = prom4)

# Se manda a llamar al data frame que contiene las medias

mediaSP

###############################################################################################################################

# Calculo de la media generalizada, considerado los valores vistos en clase

data <- c(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width)
# conjunto de datos que contiene los datos que nos interesan 
sumamg = 0
for (i in 1:600)
{
sumamg = sumamg + data[i]
}
mediaG = sumamg /600

# Se mete la media generalizada dentro de un data frame para su mejor visualización

mediaGeneralizada <- data.frame (Media_Generalizada = mediaG)

# Se manda a llamar el data frame

mediaGeneralizada

###############################################################################################################################

# Calculo de la mediana
# Los calculos se harán por cada columna para después meterse en un data frame 

proces <- function(vector1)
{
  n <- length(vector1)
  x <- 0
  while (x != 1)
{
  vector2 <- vector1 
  for (i in 1:(n-1))
{
  if (vector1[i]>vector1[i+1])
{
    vector1[i:(i+1)] <- vector1 [(i+1):i]
}
}
  x <- prod(vector2 == vector1)
}
  mediana(vector1)
} 

mediana<- function(vector1)
{
  y = length(vector1)/2
  m = vector1 [y]
  m 
}

# Todas las medianas se meten en un data frame para su mejor visualización

mediana1 <- data.frame (
                       Mediana_LongSepal  = mediana(LongSepal),
                       Mediana_AnchoSepal = mediana(AnSepal),
                       Mediana_LongPetalo = mediana(LongPet),
                       Mediana_AnchoPet   = mediana(AnPet)
                       )

# Se manda a llamar a el data frame

mediana1

###############################################################################################################################

# Calculo de la moda
# Se hará como el paso anterior 1x1

moda <- function(vector)
{
  m1 = 0
  x1 = 0
  
  for(i in 1:length(vector))
{
    vector
    x1 = 0
    for(j in 1:length(vector))
{
      if(vector[i]== vector[j] && i!=j)
{
        x1 = x1+1
}
}
    if(x1>=m1)
{
      m1 = x1
      q = i
}
}
  
  vector[q]
  
}

# Todas las modas se meten dentro de un data frame

modas <- data.frame(
                    Moda_LongSepal  = moda(LongSepal),
                    Moda_AnchoSepal = moda(AnSepal),
                    Moda_LongPetalo = moda(LongPet),
                    Moda_AnchoPet   = moda(AnPet)
                    )

# Se manda a llamar al data frame de las modas

modas

###############################################################################################################################

# Calculo de las varianzas

varianza <- function(vector)
{
smm = 0
for (i in 1:length(vector))
{
  smm = smm + vector[i]
}

p = smm / length(vector)

res = 0 
sV = 0

for (w in 1:length(vector))
{
 res = (vector[w]-smm)^2
 sV = sV + res
}

varianza1 = (1/(length(vector)-1)) * sV
}

# Se mete todo a un data frame para una mejor visualización

var <- data.frame(
                  VarLongSepal  = varianza(LongSepal),
                  VarAnchoSepal = varianza(AnSepal),
                  VarLongPetalo = varianza(LongPet),
                  VarAnchoPet   = varianza(AnPet)
                  )

# Se manda a llamar el data frame

var

###############################################################################################################################

# Calculo de la Desviación Estándar

dEstandar <- function(vector)
{
  vector = varianza (vector)
  desv = sqrt (vector)
  desv
}

# Se mete todo a un data frame para una mejor visualización

DesviacionE <- data.frame (
                           D.Estandar_LSepal = dEstandar(LongSepal),
                           D.Estandar_ASepal = dEstandar(AnSepal),
                           D.Estandar_LPetal = dEstandar(LongPet),
                           D.Estandar_APetal = dEstandar(AnPet)
)

# Se manda a llamar el Data Frame

DesviacionE

###############################################################################################################################

# Calculo de los valores máximos y mínimos de acuedo a el área de los sépalos y pétalos 

# De cada una de las 3 especies de flores Iris

Val <- function(areaPetaloo , areaSepalo){
  print("Área de Petalo:\n")
  valPetalo <- list(mini = min(areaPetaloo),máximo = max(areaSepalo))
  print(valPetalo)
  print("Área Sepalo:\n")
  valSepal <- list(mini = min(areaSepalo), máximo = max(areaPetaloo))
  print(valSepal)
}

print("Máximo y Mínimo SETOSA")
Val(setosa$areaS.area_sepal_cmcuadrados,setosa$areaS.area_petalo_cmcuadrados)
print("Máximo y Mínimo VERSICOLOR")
Val(versicolor$areaS.area_sepal_cmcuadrados,versicolor$areaS.area_petalo_cmcuadrados)
print("Máximo y Mínimo VIRGINICA")
Val(virginica$areaS.area_sepal_cmcuadrados,virginica$areaS.area_petalo_cmcuadrados)





























