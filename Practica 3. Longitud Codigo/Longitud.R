codLongitudConstante <- function(mensaje, simbolos, m) {
  # @param mensaje, numero constante de mensajes por unidad de tiempo
  # @param simbolos, alfabeto de codificacion con D simbolos
  # @param m, tamaño de la m-upla de mensajes
  
  return(ceiling(m*log2(length(mensaje))/log2(length(simbolos))))
  # Expresion matematica para codificar la m-tupla de mensajes
}

# Pruebas
cifrasDecimales = c(0:9)
simbolos = c(0,1)
codLongitudConstante(cifrasDecimales, simbolos, 3)

####################################################################

longitudMedia <- function (codificacion, pk) {
  # @param codificacion, vector de mensajes codificados
  # @param pk, vector de probabilidades
  long = 0 # Variable acumulativa del resultado
  
  for (i in 1:length(codificacion)) {
    # Recorremos los valores de la codificacion para el sumatorio
    long = long + pk[i]*nchar(codificacion[i])
    # Expresion para el calculo de la longitud media
  }
  return(long)
}

# Pruebas (ejercicio de clase, relacion de ejercicios)
mensajeCodificado = c('1','01','0000','0001','00100','00101','00110','00111')
pk = c(0.4, 0.2, 0.1, 0.1, 0.05, 0.05, 0.05, 0.05)
longitudMedia(mensajeCodificado, pk)

####################################################################################

desigualdadKarft <- function(S, simbolos, longitudes) {
  # @param S, vector de probabilidades P(A)
  # @param simbolos, alfabeto de codificacion con D simbolos
  # @param longitudes, vector de longitudes variables
  i = 1
  res = 0
  
  while (i <= length(S)) {
    # Mientras que haya mensajes...
    res = res + length(simbolos)^((-1)*longitudes[i]*i)
    # Expresion que debe cumplirse para la desigualdad de Kraft
    if (res > 1) {
      # ... comprobamos que no des-cumpla la propiedad
      break
      # Si es el caso, dejamos de comprobar
    }
    i = i + 1
    # Actualizamos i para que apunte al siguiente mensaje
  }
  return(i > length(S))
  # Si hemos salido antes de tiempo, esto deberia ser falso
  # Si se cumple la propiedad, esto es verdadero siempre
}

# Pruebas
mensajes = c(1:6)
simbolos = c(0,1)
longitudes = c(2, 2, 3, 4, 4, 5)
desigualdadKarft(mensajes, simbolos, longitudes)
