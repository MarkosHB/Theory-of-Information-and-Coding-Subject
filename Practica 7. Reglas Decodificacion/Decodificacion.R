minProbError <- function(pAi, pBj_Ai) {
  # @param pAi, vector de probabilidades P(Ai)
  # @param pBj_Ai, matriz de probabilidades P(Bj/Ai)
  
  pAiBj <- pAi*pBj_Ai # Matriz P(Ai, Bj)
  
  res <- c() # Vector con las soluciones
  index <- c() # Vector con los indices de las soluciones

  for (i in 1:ncol(pAiBj)) {
    # Buscamos en la columna...
    res <- c(res, max(pAiBj[,i]))
    # ... el mayor elemento
    index <- c(index, which.max(pBj_Ai[,i]))
    # ... y su posicion
  }
  return(list(res, index))
}

maxVerosimilitud <- function(pBj_Ai) {
  # @param pBj_Ai, matriz de probabilidades P(Bj/Ai)
  
  res <- c() # Vector con las soluciones
  index <- c() # Vector con los indices de las soluciones
  
  for (i in 1:ncol(pBj_Ai)) {
    # Buscamos en la columna...
    res <- c(res, max(pBj_Ai[,i]))
    # ... el mayor elemento
    index <- c(index, which.max(pBj_Ai[,i]))
    # ... y su posicion
  }
  return(list(res, index))
}

# Pruebas
pAi <- c(0.5, 0.5)
pBj_Ai <- matrix(c(c(1/2,1/6), c(1/4,1/3), c(1/4,1/2)), ncol=3) 
minProbError(pAi, pBj_Ai)
maxVerosimilitud(pBj_Ai)
