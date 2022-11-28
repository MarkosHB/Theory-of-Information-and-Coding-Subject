# Autor: Marcos Hidalgo Baños

entropiaDosValores <- function(p) {
  
  if (p > 1 || p < 0) {NA}
  #Comprobamos que 'p' es un valor de probabilidad válido
  
  else if (p == 1 || (1-p) == 1) {0} 
  #La entropía en los casos extremos siempre es 0
  
  else {-(p*log2(p) + (1-p)*log2(1-p))}
  #Expresión de la entropía aplicada a dos valores
  
}

calcEntropia = function(p){-(p*log2(p) + (1-p)*log2(1-p))}
# Extracción de la fórmula del cálculo de la entropía

plot(calcEntropia, from=0, to=1,
     xlab="Probabilidad del suceso", ylab="Valor entropía", 
     xlim=c(0,1), ylim=c(0,1), main="Cálculo de la entropía para dos valores")
# Graficamos en el rango, ajustamos los límites y etiquetamos el gráfico con sus ejes

#########################################################

p = c(0.5, 0.25, 1/8, 1/16, 1/16)
# Vector de probabilidades de ejemplo

entropia <- function(p_i) {
  
  if (sum(p_i) != 1) {NA}
  # La sumatoria de los valores debe ser 1
  
  else {sum(p_i*log2(1/p_i))}
  # Expresión general de la entropía
  
}


