entropiaCompuesta <- function(pAi, pBj_Ai) {
  # @param pAi, vector de probabilidades P(A)
  # @param pBj_Ai, matriz de probabilidades P(Bj/Ai)
  
  res = 0 # Variable acumulativa 'resultado'
  pAiBj <- pAi*pBj_Ai # Matriz P(Ai, Bj)
  
  for (i in 1:nrow(pAiBj)) {
    # Recorremos las filas de pAiBj para el sumatorio
    for (j in 1:ncol(pAiBj)) {
      # Recorremos las columnas de pAiBj para el sumatorio
      if (pAiBj[i,j] == 0 | sum(pAiBj) != 1) {
        NA
        # si los datos no son correctos, devolvemos NA
      } else {
        res = res + pAiBj[i,j]*log2(1/pAiBj[i,j])
        # expresion matematica de la entropia compuesta
      }
    }
  }
  
  return(res)
}


#Pruebas
pAi <- c(0.5, 0.5)
pBj_Ai <- matrix(c(c(1/4,1/4,1/4,1/4), c(1,0,0,0)), ncol=2) 
# Importante el ncol=2 del final, si no se pone falla la condicionada

entropiaCompuesta(pAi, pBj_Ai)



entropiaCondicionadaY_X <- function(pAi, pBj_Ai) {
  # @param pAi, vector de probabilidades P(A)
  # @param pBj_Ai, matriz de probabilidades P(Bj/Ai)
  
  res = 0 # Variable acumulativa 'resultado'
  pAiBj <- pAi*pBj_Ai # Matriz P(Ai, Bj)
  
  for (i in 1:nrow(pBj_Ai)) {
    # Recorremos las filas de pBj_Ai para el sumatorio
    for (j in 1:ncol(pBj_Ai)) {
      # Recorremos las columnas de pBj_Ai para el sumatorio
      if (pBj_Ai[i,j] == 0 | sum(pAiBj) != 1) {
        NA
        # si los datos no son correctos, devolvemos NA
      } else {
        res = res + pAiBj[i,j]*log2(1/pBj_Ai[i,j])
        # expresion matematica de la entropia compuesta
      }
    }
  }
  
  return(res)
}

entropiaCondicionadaX_Y <- function(pAi, pBj_Ai) {
  # @param pAi, vector de probabilidades P(A)
  # @param pBj_Ai, matriz de probabilidades P(Bj/Ai)
  
  res = 0 # Variable acumulativa 'resultado'
  pAiBj <- pBj_Ai*pAi # Matriz P(Ai, Bj)
  pBj <- Bj(pAi, pBj_Ai) # Vector de prob. P(B)
  pAi_Bj <- ((pBj_Ai*pAi)/pBj) # Matriz de prob P(Ai/Bj)
  
  for (i in 1:nrow(pAi_Bj)) {
    # Recorremos las filas de pAi_Bj para el sumatorio
    for (j in 1:ncol(pAi_Bj)) {
      # Recorremos las columnas de pAi_Bj para el sumatorio
      if (pAi_Bj[i,j] == 0 | sum(pAiBj) != 1) {
        NA
        # si los datos no son correctos, devolvemos NA
      } else {
        res = res + pAiBj[i,j]*log2(1/pAi_Bj[i,j])
        # expresion matematica de la entropia compuesta
      }
    }
  }
  
  return(res)
}


# Pruebas
entropiaCondicionadaY_X(pAi, pBj_Ai)
entropiaCondicionadaX_Y(pAi, pBj_Ai)



informacionMutua <- function(pAi, pBj_Ai) {
  # @param pAi, vector de probabilidades P(A)
  # @param pBj_Ai, matriz de probabilidades P(Bj/Ai)
  
  res <- list(entropia(pAi) - entropiaCondicionadaX_Y(pAi, pBj_Ai),
              entropia(Bj(pAi, pBj_Ai)) - entropiaCondicionadaY_X(pAi, pBj_Ai))
  # Almacenamos las dos formas de calcular la IM para corroborar que son iguales
  
   return(res)
}

#Pruebas
informacionMutua(pAi, pBj_Ai)


#Funciones auxiliares

Bj <- function(pAi, pBj_Ai) {
  # @param pAi, vector de probabilidades P(A)
  # @param pBj_Ai, matriz de probabilidades P(Bj/Ai)
  
  pBj <- c() # Matrix resultado P(B)
  
  for(i in 1:nrow(pBj_Ai)) {
    # Recorremos las filas de pBj_Ai
    pBj <- c(pBj, sum(pAi*pBj_Ai[i,]))
    # Expresion matematica de la prob. condicionada
  }
  
  return(pBj)
}

entropia <- function(p_i) {
  
  if (sum(p_i) != 1) {NA}
  # La sumatoria de los valores debe ser 1
  
  else {sum(p_i*log2(1/p_i))}
  # Expresión general de la entropía
  
}

  
  