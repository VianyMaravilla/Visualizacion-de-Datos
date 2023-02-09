# Proyecto 04
# Vianey Maravilla Pérez
# 5AM1
# Analítica y Visualización de Datos

# Considerar los datos económicos de la siguiente tabla (dados en millones de dólares) de corporaciones industriales



# 1. Calcular la distancia de Mahalanobis entre Ford y Exxon
# 2. Calcular la distancia de Mahalanobis entre General Motors e IBM
# 3. Calcular la distancia de Mahalanobis entre Philip Morris y Texaco




ruta <- file.choose()
main <- function(ruta) {
  print(ruta)
  datos<- read.csv(ruta, 
                   header = T,
                   sep = ",")
  row.names(datos) <- datos[,1]
  datos <- datos[,2:dim(datos)[2]]
  View(datos)
  means_data <- means(datos)
  print("Means")
  print(means_data)
  covars_data <- covars(datos) 
  print("Covars")
  print(covars_data)
  print("Distancia de Mahalanobis entre Ford y Exxon")
  FE_D <- mahalanobis_d(datos["Ford",], datos["Exxon",], covars_data)
  print(FE_D)
  print("Distancia de Mahalanobis entre General Motors e IBM")
  GMIBM_D <- mahalanobis_d(datos["General Motors",], datos["IBM",], covars_data)
  print(GMIBM_D)
  print("Distancia de Mahalanobis entre Philip Morris y Texaco")
  MT_D <- mahalanobis_d(datos["Philip Morris",], datos["Texaco",], covars_data)
  print(MT_D)
}

means <- function(datos) {
  col_no <- dim(datos)[2]
  row_no <- dim(datos)[1]
  means_list <- list()
  for(i in 1:col_no) {
    suma <- 0
    for(j in 1:row_no) {
      suma <- suma + datos[j,i]
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

covars <- function(datos) {
  cov_mat <- cov(datos)
  return(cov_mat)
}

mahalanobis_d <- function(vector1, vector2, covars_data) {
  dis_vect <- vector1 - vector2
  covars_data_inv <- solve(covars_data)
  dis_vect_covars_inv <- mult_mat(dis_vect, covars_data_inv)
  t_dis_vect <- t(dis_vect)
  mahalanobis_dis <- mult_mat(dis_vect_covars_inv, t_dis_vect)
  return(mahalanobis_dis)
}

main(ruta)

