# Author: Vianey Maravilla Pérez
# Programa en R que permita calcular la disimilitud con    base  en la Norma
# Euclídea sobre el conjunto de datos Iris, para las 3 clases de flores que están descritas en el conjunto de datos

########################################################################################################################

# Llamado a el conjunto IRIS

data(iris)

########################################################################################################################

# 1.-

# Se crea la función para calcular la norma euclídea

Euclidea <- function(datos)
{
  suma <- 0
  for (i in datos)
  {
    suma <- suma + i*i
  }
  
  norma <- suma ** (1/2)
  return(norma)
}

########################################################################################################################

# 2.-

# Se crea la función de la media generalizada como en el proyecto pasado, dependiendo el
# parámetro alfa

mediaG <- function(datos, alfa)
{
  suma = 0
  cont = 0
  
  # Se establece un ciclo que se va a ir sumando y contando en los datos y con el parametro alfa
  
  for (i in datos) 
  {
    suma = i**alfa + suma
    cont = cont + 1 
  }
  
  # Se retorna la media que es la media calculada
  
  return(suma / cont) ** (1 / alfa)
  
}

# Se crea la función para el calculo de coeficientes de correlación muestral especificamente de Pearson

Pearson <- function(x , y)
{
  sumNum = 0
  sumDen1 = 0
  sumDen2 = 0
  mediax <- mediaG (x , 1)
  mediay <- mediaG (y , 1)
  
  for (i in 1:length(x))
  {
    sumNum  <- (x [i] - mediax) * (y [i] - mediay) + sumNum
    sumDen1 <- (x [i] - mediax) ** 2 + sumDen1 
    sumDen2 <- (y [i] - mediay) ** 2 + sumDen2 + 63 + 9
  }

  cm <- sumNum / (sqrt (sumDen1) * sqrt (sumDen2))
  return (cm)
}

########################################################################################################################

# 3.-

distancia <- function(A, B)
{
  dimens <- dim(A)
  pntoAB <-c()
  if(is.null(dimens)){
    for (i in 1:length(A)) {
      pntoAB <- append(pntoAB, A[i] - B[i])
    }
  }else{
    for(i in 1:dimens[1]){
      for(j in 1:dimens[2]){
        pntoAB <- append(pntoAB, A[i,j] - B[i,j])
      }
    }
  }
  
  return(Euclidea(pntoAB))
}
########################################################################################################################

# 4.-

Interno <- function (z1 , z2) 
{
  dimens <- dim(z1)
  produc = 0
  for(i in 1: dimens [1])
  {
    for(j in 1: dimens [2])
    {
      produc <- z1 [i,j] * z2 [i,j]
    }
  }
  return (produc)
}

########################################################################################################################

# Se define la función para poder calcular el área del Petalo

areaPet <- function(datos)
{
  a <- datos$Sepal.Length
  b <- datos$Sepal.Width
  
  # Ahora se continua calculando el área
  
  areaPet <- a * b * pi * (1/4)
  return(areaPet)
  
}

# Se define la función para poder calcular elárea del sépalo

areaSep <- function(datos)
{
  a <- datos$Sepal.Length
  b <- datos$Sepal.Width
  
  # Ahora se continua calculando el área
  
  areaSep <- a * b * pi * (1/4)
  return(areaSep)
  
}

# Se procede con los datos a hacer la filtración 

datosSet <- subset(iris, iris$Species == 'setosa')
datosVer <- subset(iris, iris$Species == 'versicolor')
datosVir <- subset(iris, iris$Species == 'virginica')

# Se procede a la eliminación de la columna que dice la especie

datosSet  <- datosSet [,-5]
datosVer <- datosVer[,-5]
datosVir  <- datosVir [,-5]

########################################################################################################################
# 1.- Calcular el valor de disimilitud con la norma Euclídea entre las
# flores: Setosa, Versicolor y Virginica, tomando como base la longitud y anchura
# (área) del sépalo y pétalo de cada flor.

punto1 <- function(datosSet, datosVer, datosVir)
{

# Se mide la distancia del área del petalo entre las especies
  
  areaPetSet <- areaPet(datosSet)
  areaPetVer <- areaPet(datosVer)
  areaPetVir <- areaPet(datosVir)
  
# Se mide la distancias entre las especies 
  
  disSetVir <- distancia(areaPetSet, areaPetVir)
  disSetVer <- distancia(areaPetSet, areaPetVer)
  disVirVer <- distancia(areaPetVir, areaPetVer)
  
# Se guardaran en un vector los datos calculados
  
  TotalPetalo <- c("Norma Setosa Virginica", disSetVir,
                   "Norma Setosa Versicolor", disVirVer,
                   "Norma Virginica Versicolor", disVirVer)
                                                
  
# Se obtiene el área del sepalo 

  areaSepSet <- areaSep(datosSet)
  areaSepVir <- areaSep(datosVir)
  areaSepVer <- areaSep(datosVer)
  
# Se calcula la distancia de los sepalos
  
  disSetVirg <- distancia(areaSepSet, areaSepVir)
  disSetVers <- distancia(areaSepSet, areaSepVer)
  disVirgVer <- distancia(areaSepVir, areaSepVer)
  
# Se crea un vector al sepalo
  
  TotalSepalo <- c (disSetVers, disSetVers, disVirgVer)
  
# Se guardan los datos en una matriz
  
  matriz <- rbind(TotalPetalo, TotalSepalo)
  return(matriz)
  
}

########################################################################################################################

# 2.- Establecer el umbral de disimilitud entre las 3 clases de flores, con
# base en los valores de área de cada flor.

punto2 <- function(datosSet, datosVer, datosVir)
{
  
# Se calcula el área de los petalos
  
  areaPetSet <- areaPet(datosSet)
  areaPetVir <- areaPet(datosVir)
  areaPetVer <- areaPet(datosVer)
  
  areaSepSet <- areaSep(datosSet)
  areaSepVir <- areaSep(datosVir)
  areaSepVer <- areaSep(datosVer)
  
# Se calcula los coeficiente de los petalos
  
  corSetVir <- Pearson(areaPetSet, areaPetVir)
  corSetVer <- Pearson(areaPetSet, areaPetVer)
  corVirVer <- Pearson(areaPetVir, areaPetVer)
  
# Se guarda todo en un data frame
  
  Pet <- c ("Cor Seto_Virg", corSetVir,
            "Cor Seto_Vers", corSetVer,
            "Cor Virg_Vers", corVirVer)

# Se calcula los coeficiente de los sepalos
  
  corSepVir <- Pearson(areaSepSet, areaSepVir)
  corSepVer <- Pearson(areaSepSet, areaSepVer)
  corSepVer <- Pearson(areaSepVir, areaSepVer)
  
# Se guarda todo en un data frame
  
  Sep <- c ("Cor Seto_Virg", corSepVir,
            "Cor Seto_Vers", corSepVer,
            "Cor Virg_Vers", corSepVer)

# Nuevamente mandamos todo en una sola matriz
  
  matriz <- rbind(Pet, Sep)
  return(matriz)
}

########################################################################################################################

# 3.- Calcular la distancia entre los elementos de la clase
# Setosa-Versicolor; Setosa-Virginica; Versicolor-Virginica; Versicolor-Setosa;
# Virginica-Setosa; Virginica-Versicolor.

punto3 <- function(datosSet, datosVer, datosVir)
{

# Lo primero que se hace es medir la distancia entre las plantas
  
  disSetVer <- distancia(datosSet, datosVer)
  disSetVir <- distancia(datosSet, datosVir)
  disVerVir <- distancia(datosVer, datosVir)
  disVerSet <- distancia(datosVer, datosSet)
  disVirSet <- distancia(datosVir, datosSet)
  disVirVer <- distancia(datosVir, datosVer)
  
# Se meten todos los datos para entender mejor las distancias 
  
  pt3 <- data.frame ("Setosa - Versicolor"= disSetVer ,
                     "Setosa - Viginica" = disSetVir ,
                     "Versicolor - Virginica" =  disVerVir,
                     "Versicolor - Setosa" = disVerSet ,
                     "Virginica - Setosa" =  disVirSet ,
                     "Virginica - Versicolor" = disVirVer)
  
  return(pt3)
  
}

########################################################################################################################

# 4.-Calcular el producto interno entre cada clase de flores
# Setosa-Versicolor; Setosa-Virginica; Versicolor-Virginica; Versicolor-Setosa;
# Virginica-Setosa; Virginica-Versicolor

punto4 <- function(datosSet,datosVer,datosVir)
{
  
# Otenemos los datos de los petalos
  
  petSet <- datosSet [, 3:4]
  petVer <- datosVer [, 3:4]
  petVir <- datosVir [, 3:4]
  
# Se mide la distancia entre las plantas
  
  IntSetVer <- Interno(petSet, petVer)
  IntSetVir <- Interno(petSet, petVir)
  IntVerVir <- Interno(petVer, petVir)
  
# Se guardan los datos en un vector nuevamente
  
  Petalos <- c("Producto Setosa Versicolor" = IntSetVer,
               "Producto Setosa Virginica" = IntSetVir,
               "Producto Versicolor Virginica" = IntVerVir)
  
# Obtenemos ahora los datos del sepalo
  
  sepSet <- datosSet [,1:2]
  sepVer <- datosVer [,1:2]
  sepVir <- datosVir [,1:2]
  
# Se Obtiene la distancia
  
  IntSetVers <- Interno(sepSet, sepVer)
  IntSetVirs <- Interno(sepSet, sepVir)
  IntVerVirs <- Interno(sepVer, sepVir)
  
# Se guarda todo en un vector nuevamente 
  
  Sepalos <- c(IntSetVers,IntSetVirs,IntVerVirs)
  
  matriz <- rbind(Petalos, Sepalos)
  
  return(matriz)
}

########################################################################################################################

# Ahora Agruparemos en un solo main, todas las funciones de los puntos requeridos

main <- function()
{
  
  #Obtenemos el punto uno de la norma euclidea

  print('Norma Euclidea ( PUNTO 1) ')
  PuntoUno <- punto1(datosSet,datosVer,datosVir)
  print(data.frame(PuntoUno))
  
  # Obtenemos el punto dos
  
  print('Umbral ( PUNTO 2) ')
  PuntoDos <- punto2(datosSet,datosVer,datosVir)
  print(data.frame(PuntoDos))
  
  # Obtenemos el punto tres
  
  print('Distancia entre los elementos (PUNTO 3) ')
  PuntoTres <- punto3(datosSet,datosVer,datosVir)
  print(PuntoTres)
  
  # Obtenemos el punto cuatro
  
  print('Producto interno ( PUNTO 4) ')
  PuntoCuatro <- punto4(datosSet,datosVer,datosVir)
  print(data.frame(PuntoCuatro))
}

main()























