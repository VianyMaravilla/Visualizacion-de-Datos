

#Importar el conjunto de datos "Iris"
data("iris")
names(iris)
table(iris)

#Conversion de cada columna en vector
vec1<-c(iris$Sepal.Length)
vec2<-c(iris$Sepal.Width)
vec3<-c(iris$Petal.Length)
vec4<-c(iris$Petal.Width)
vec5<-c(iris$Petal.Width)

#Muestra de cada columna hecha vector
table(vec1)
table(vec2)
table(vec3)
table(vec4)
table(vec5)

#Media de los valores de cada columna
a<-sum(vec1)/150
b<-sum(vec2)/150
c<-sum(vec3)/150
d<-sum(vec4)/150
e<-sum(vec5)/150

#Impresion de los valores seleccionados

print(paste("La media de la primer columna es",a))
print(paste("La media de la segunda columna es",b))
print(paste("La media de la tercer columna es",c))
print(paste("La media de la cuarte columna es",d))
print(paste("La media de la quinta columna es",e))

#Correlacion entre columans

a1<-(a/b)
b1<-(c/d)

print(paste("La correlacion del Sepalo es:",a1))
print(paste("La correlacion del Petalo es:",b1))

#Preguntas
print("Pregunta 1")
print("¿Cual de los datos podria contener errores o asignaciones de clases falsa")
print("Si es un valor atipico puede ser considerado de clase falsa")
print("Pregunta 2")
print("¿Cual es el error causado por redondear los datos a un decimal")
print("Si se empieza a redondear desde un principio, esto genera que el resultado final sea muy alejado con respecto al que sale al no redondear")
print("Pregunta 3")
print("¿Cual es la correlacion entre la longitud y el ancho de los petalos")
print(paste("La correlacion es",b1))
print("Pregunta 4")
print("¿Que par de dimensiones estan mas correlacionadas?")
print("En el petalo es donde existe menor variacion en las correlaciones")
print("Pregunta 5")
print("Ninguna de las flores en el dataset tiene un ancho de sepalo de 1.8' ¿Que longitud de sepalo esperariamos para que una flor tuviera 1.8cm de ancho de sepalo")
flor<-1.8*a1
print(paste("La longitud que deberia de tener es de",flor))
print("Pregunta 6")
print("¿A que especie perteneceria un Iris con un ancho de sepalo de 1.8cm")
print("A la especie que mas se acercaria seria la Setosa")
print("Pregunta 7")
print("Las 3 especies contienen subespecies que pueden identificarse apartir de los datos?")
print("Si, ya que ciertos datos se muestran como anomalos o outlayers, los cuales bien se pueden clasificar dentro de un subconjunto")