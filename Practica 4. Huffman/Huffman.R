HuffmanGeneralizado <- function(mensajes, simbolos, probabilidades) {
  # @param mensajes, vector de mensajes a codificar
  # @param simbolos, vector con los simbolos del alfabeto
  # @param probabilidades, vector de probabilidades de cada mensaje
  
  # PASO 1.
  if (sum(probabilidades) != 1) {
    # Comprobamos que las probabilidades suman exactamente uno
    return('ERROR. Las probabilidades no suman 1.')
    # Si es el caso, debemos terminar inmediatamente
  } 
  ordenado = OrdenarVector(probabilidades)
  # Ordenamos las probabilidades descendentemente
  
  # PASO 2.
  n = length(mensajes) # numero de mensajes a codificar
  D = length(simbolos) # total de simbolos para codificar
  r = (n-2)%%(D-1) # resto de la division entera
  
  # PASO 3.
  codigo <- vector(mode = "character", length = n) 
  # Vector solucion (ahora vacio) con la codificacion asignada
  
  i = n-(r+1) # Puntero a los ultimos mensajes a codificar
  s = D-(r+1) # Puntero a los simbolos a escribir en esta iteracion
  p_ac = 0    # Nueva probabilidad acumulada de los ultimos mensajes
  m = 0
  
  while (length(ordenado) != 1) {
    # Acabaremos cuando el vector de prob. ordenado sea un solo valor
    posiciones = c(i:n)
    
    # if (length(mensajes) != length(ordenado) ) {
    #   m = max(ordenado[posiciones])
    #   s = ((n-m) %% D) + 1
    # }
    
    for (j in posiciones) {
      # Para cada mensaje a partir de i hasta n
      
      if (codigo[j] == "") 
        { # Si es la primera iteracion del asignado
          codigo[j] = simbolos[s]
          # Escribimos el simbolo correspondiente
        } 
      else 
        { # Si no, pegamos delante el nuevo simbolo
        for (k in n:length(codigo)) {
          # Esto debe realizarse a todos los sucesivos simbolos
          codigo[k] = paste(simbolos[s], codigo[k])
          # Pegamos el nuevo simbolo delante del que ya estaba
        }
      }
      print(codigo)
      
      if (s < D) {
        # Avanzamos al siguiente simbolo
        s = s + 1
      } else {
        # O comenzamos desde el primer simbolo
        s = ((s+1) %% D) + 1
      }
   
      p_ac = p_ac + ordenado[j]
      # Acumulamos las prob. de los mensajes parcialmente codificados
    }

    # PASO 4.
    ordenado = OrdenarVector(ReagruparVector(probabilidades, i, p_ac))
    # Recalculamos el nuevo vector ordenado a partir de la prob. acumulada
    print(ordenado)    
    
    p_ac = 0  # Reseteo de la variable acumulativa de probabilidades
    n = length(ordenado)  # Actualizacion de la longitud tras el reagrupe
    i = n - (D-1) # Colocacion del indice sobre la siguiente 
    s = 1 # Colocamos el puntero en el primer simbolo

  }

  return(codigo)
}

OrdenarVector <- function(probabilidades) {
  # @param probabilidades, vector de probabilidades
  
  return(rev(sort(probabilidades)))
  # Ordena descendentemente el vector
}

ReagruparVector <- function(probabilidades, i, p_ac) {
  # @param probabilidades, vector original a reducir
  # @param i, indice que indica el tope superior
  
  new_vector = probabilidades[-c(i:length(probabilidades))]
  # Recortamos el resto de probabilidades que colapsan en una
  new_vector[i] = p_ac
  # La ultima posicion sera la acumulada de las sucesivas (que no estaran)
  return(new_vector)
}

# Pruebas con el ejemplo hecho en clase de Huffman ternario
mensajes = c('a1','a2','a3','a4','a5','a6','a7','a8')
probabilidades = c(0.4,0.2,0.1,0.1,0.05,0.05,0.05,0.05)
simbolosTernario = c('0','1','2')
HuffmanGeneralizado(mensajes, simbolosTernario, probabilidades)



# solucion -> (0, 2, 11, 12, 101, 102, 1001, 1002)


#  Estructura de datos mayor, puede fallar en ejemplos de mayor tamaño

