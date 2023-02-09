# Vianey Maravilla Pérez
# 5AM1
# Analítica Y Visualización de Datos

# Proyecto 3
# Descripción:
# Programa en R que permita calcular la distancia de Mahalanobis sobre el conjunto de datos Iris, para las 3 clases
# de flores que están descritas en el conjunto de datos.

# Cuantificar el valor de la distancia de Mahalanobis entre las flores: Setosa, Versicolor y Virginica, 
# tomando como base la longitud y anchura del sépalo y pétalo de cada flor.

# Graficar los valores de las distancias calculadas.



main <- function() {
  data <- data.frame(iris)
  flower_type = levels(factor(data$Species))
  setosa <- data.frame(data[data$Species == flower_type[1], c(1, 2, 3, 4, 5)])
  versicolor <- data.frame(data[data$Species == flower_type[2], c(1, 2, 3, 4, 5)])
  virginica <- data.frame(data[data$Species == flower_type[3], c(1, 2, 3, 4, 5)])
  View(setosa)
  View(versicolor)
  View(virginica)
  setosaD<- mahalanobis_dist(setosa[,1:4])
  versicolorD<- mahalanobis_dist(versicolor[,1:4])
  virginicaD<- mahalanobis_dist(virginica[,1:4])
  print(" Mahalanobis Setosa D.")
  print(setosaD)
  graficate(setosaD, "Mahalanobis Setosa D.")
  print("versicolor_mahalanobis")
  print(versicolorD)
  graficate(versicolorD, "Mahalanobis versicolor D.")
  print("virginica_mahalanobis")
  print(virginicaD)
  graficate(virginicaD, "Mahalanobis Virginica")
}

means <- function(data) {
  col_no <- dim(data)[2]
  row_no <- dim(data)[1]
  means_list <- list()
  for(i in 1:col_no) {
    suma <- 0
    for(j in 1:row_no) {
      suma <- suma + data[j,i]
    }
    means_list <- c(means_list, suma / row_no)
  }
  return(means_list)
}

mult_mat <- function(mat1, mat2) {
  row_no1 <- dim(mat1)[1]
  row_no2 <- dim(mat2)[1]
  col_no1 <- dim(mat1)[2]
  col_no2 <- dim(mat2)[2]
  if(col_no1 == row_no2) {
    new_mat <- data.frame()
    for(a in 1:col_no2) {
      rown <- c()
      for(i in 1:row_no1) {
        suma <- 0
        for(j in 1:col_no1) {
          suma <- suma + (mat1[i,j] * mat2[j,a])
        }
        rown <- c(rown, suma)
      }
      if(dim(new_mat)[1] == 0) {
        new_mat <- cbind(rown)
      } else {
        new_mat <- cbind(new_mat, rown)
      }
    }
    return(new_mat)
  }
}

mahalanobis_dist <- function(data) {
  cov_matrix <- cov(data)
  means_data <- means(data)
  row_no <- dim(data)[1]
  col_no <- dim(data)[2]
  identity_matrix <- diag(row_no)
  ones_matrix <- matrix(1, row_no, row_no)
  identity_one <- identity_matrix - ((1 / row_no) * ones_matrix)
  t_data <- t(data)
  cov_matrix_inv <- solve(cov_matrix)
  first_step <- mult_mat(identity_one, data)
  second_step <- mult_mat(first_step, cov_matrix_inv)
  third_step <- mult_mat(second_step, t_data)
  four_step <- mult_mat(third_step, identity_one)
  mahalanobis_dists <- diag(four_step)
  return(mahalanobis_dists)
}

graficate <- function(data, name) {
  plot(density(data, bw = 0.5),
       main="Ditancias Mahalanobis") ; rug(data)
  qqplot(qchisq(ppoints(length(data)), df = 3), data,
         main = expression("Q-Q ploteo de Magalanobis quantiles" * ~ chi[3]^2))
  abline(0, 1, col = 'red')
}

main()
