# Tarea 2: Conjunto Iris
# Vianey Maravilla Pérez
# 5AM1

# Se importaran los datos del conjunto llamado "Iris" con el cual trabajaremos

data("iris")
names(iris)
table(iris)

# Se convertira cada columna del conjunto en un vector con su respectivo nombre

v1<-c(iris$Sepal.Length)
v2<-c(iris$Sepal.Width)
v3<-c(iris$Petal.Length)
v4<-c(iris$Petal.Width)
v5<-c(iris$Petal.Width)


# Muestra de cada vector que antes era una columna

table(v1)
table(v2)
table(v3)
table(v4)
table(v5)

# Se saca las medias de los valores de cada comlumna (vector) y se imprimen 

m1<-sum(v1)/150
m2<-sum(v2)/150
m3<-sum(v3)/150
m4<-sum(v4)/150
m5<-sum(v5)/150
print(paste("Media de la primer columna (vector):",m1))
print(paste("Media de la segunda columna (vector):",m2))
print(paste("Media de la tercer columna (vector):",m3))
print(paste("Media de la cuarte columna (vector):",m4))
print(paste("Media de la quinta columna (vector):",m5))

# Se implementa la correlación entre las columnas (Vectores)

c1<-(m1/m2)
c2<-(m3/m4)
print(paste("La correlacion del Sepalo es:",c1))
print(paste("La correlacion del Petalo es:",c2))

# Se saca la longitud esperada para una flor de 1.8 cm de ancho de sepalo
longifl<-1.8*c1

# Ahora se responde a las preguntas cuestionadas dentro del trabajo

# Pregunta 1:
print("¿Cual de los datos podria contener errores o asignaciones de clases falsa")
print("Cualquier valor atipico puede ser considerado de clase falsa")

# Pregunta 2:
print("¿Cual es el error causado por redondear los datos a un decimal")
print("Que se genere un resultado alejado a comparación al que no se rendondea")

# Pregunta 3
print("¿Cual es la correlacion entre la longitud y el ancho de los petalos")
print(paste("La correlacion entre la longitud y el ancho de los petalos es la siguiente:\n",c2))

# Pregunta 4
print("¿Que par de dimensiones estan mas correlacionadas?")
print("Existe más correlación donde la variación no es tan inestable")

# Pregunta 5
print("Ninguna de las flores en el dataset tiene un ancho de sepalo de 1.8' ¿Que longitud de sepalo esperariamos para que una flor tuviera 1.8cm de ancho de sepalo")
print(paste("La longitud que deberia de tener es de",longifl))

# Pregunta 6 
print("¿A que especie perteneceria un Iris con un ancho de sepalo de 1.8cm")
print("Pertenecería o se acercaría más a la setosa")

#Pregunta 7
print("¿Las 3 especies contienen subespecies que pueden identificarse apartir de los datos?")
print("Si, puesto que dentro de ciertos datos existen anomalias y outlayers y eso puede caer en un subconjunto el cual 
      se puede clasificar como una subespecies")

