calcCapacidad <- function(probabilidades) {
  # @param probabilidades, matriz de prob. condicionadas
  
  H_i = c() # Variable para el resultado
  
  for (i in 1:nrow(probabilidades)) {
    # Recorremos los valores de la matriz por filas...
    H_i[i] = entropia(probabilidades[i,])
    # ... para calcular la entropia asociada
  }
  
  print("Valores de salida de H_i")
  print(H_i) # Mostramos el resultado
  
  solucion = solve(probabilidades, H_i)
  # Solucionamos el sistema 
  print("Solucion del sistema de ecuaciones")
  print(solucion) # Mostramos el resultado
  
  capacidad = log2(sum(1/2^solucion))
  # Implementamos la expresion matematica
  print("Capacidad del canal")
  print(capacidad) # Mostramos el resultado
  
  return(solucion) # Lo devolvemos para usarlo en la otra funcion
  
}

calcDistribucionInicial <- function(probabilidades, solucion) {
  # @param probabilidades, matriz de prob. condicionadas
  # @param solucion, vector de soluciones del sistema
  
  t_probabilidades = t(probabilidades)
  # matriz transpuesta de la original
  
  B_j = c() # Vector de las yj = p(Bj)
  total = sum(1/2^solucion) # Sumatorio
  for (j in 1:length(solucion)) {
    # Para cada elemento...
    B_j[j] = 2^(-solucion[j])/total
    # calculamos B_j a partir de la expresion matematica
  }
  
  print("Valores calculados yj = p(Bj)")
  print(B_j) # Mostramos el resultado
  
  solucion = solve(t_probabilidades, B_j)
  # Solucionamos el sistema 
  print("Solucion del sistema de ecuaciones p(A_i)")
  print(solucion) # Mostramos el resultado
  
}

# Pruebas (ejemplos de las transparencias)
probabilidades = 
  matrix(c(c(1/2,0,5/8,0),c(1/2,0,3/8,0),c(0,1/4,0,1/2),c(0,3/4,0,1/2)),ncol=4)

sol = calcCapacidad(probabilidades)
calcDistribucionInicial(probabilidades, sol)


#####################################################
# Funciones auxiliares

entropia <- function(p_i) {
  
  if (sum(p_i) != 1) {NA}
  # La sumatoria de los valores debe ser 1
  
  else {sum(p_i*log2(1/p_i), na.rm = TRUE)}
  # Expresión general de la entropía
  
}
