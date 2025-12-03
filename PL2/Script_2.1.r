### =======================
###  DATOS DEL EJERCICIO
### =======================

# Matriz con los 15 pares {Velocidad, Temperatura}
# Se cargan por filas (byrow=TRUE)
datos <- matrix(c(
  3.5,4.5,
  0.75,3.25,
  0,3,
  1.75,0.75,
  3,3.75,
  3.75,4.5,
  1.25,0.75,
  0.25,3,
  3.5,4.25,
  1.5,0.5,
  1,1,
  3,4,
  0.5,3,
  2,0.25,
  0,2.5),
  ncol=2, byrow=TRUE)

colnames(datos) <- c("Velocidad","Temp")

K <- 3        # Número de clusters
n <- nrow(datos)


### =========================
### FUNCIONES Básicas
### =========================


# Suma
suma <- function(v){
  total <- 0
  for(i in 1:length(v)){
    total <- total + v[i]
  }
  return(total)
}

# Índice del mínimo
which_min <- function(v){
  indice <- 1
  valor  <- v[1]

  for(i in 2:length(v)){
    if(v[i] < valor){
      valor <- v[i]
      indice <- i
    }
  }
  return(indice)
}

# Media hecha a mano
media <- function(v){
  return(suma(v) / length(v))
}

# Distancia euclidiana dist = sqrt( (x1-x2)^2 + (y1-y2)^2 )
dist_euclidea <- function(p, c){
  v <- (p - c)^2
  return(sqrt(suma(v)))
}

### ======================================
### INICIALIZACIÓN FLEXIBLE DE CENTROIDES
### ======================================

inicializar_centroides <- function(datos, K, manual = NULL){

  # --- OPCIÓN 1: pasas los centroides manualmente ---
  if(!is.null(manual)){
    return(manual)
  }

  # --- OPCIÓN 2: centroides aleatorios dentro del rango de los datos ---

  # Para cada columna (velocidad y temperatura), obtenemos rango
  mins <- apply(datos, 2, min)
  maxs <- apply(datos, 2, max)

  centroides <- matrix(0, nrow=K, ncol=ncol(datos))

  for(k in 1:K){
    for(j in 1:ncol(datos)){
      # número aleatorio uniforme dentro del rango de la columna
      centroides[k, j] <- runif(1, mins[j], maxs[j])
    }
  }

  return(centroides)
}


### ======================================
### 1. Centroides iniciales (Paso A.1)
### ======================================

### → O bien pones tus centroides
centroides <- inicializar_centroides(datos, K, manual = datos[c(1,4,8), ])

### → O bien los generas aleatoriamente (más genérico)
#centroides <- inicializar_centroides(datos, K)


### ======================================
### Asignación de puntos al cluster más cercano Paso A.2 / B.2
### ======================================
asignar_clusters <- function(datos, centroides){
  n <- nrow(datos)
  K <- nrow(centroides)
  asignacion <- rep(0, n)

  for(i in 1:n){
    distancias <- rep(0, K)

    # Calculamos distancia desde el punto i a cada centroide
    for(k in 1:K){
      distancias[k] <- dist_euclidea(datos[i,], centroides[k,])
    }

    # Elegimos el centroide más cercano
    asignacion[i] <- which_min(distancias)
  }

  return(asignacion)
}


### ======================================
### Recalcular centroides (Paso B.1)
### ======================================
recalcular_centroides <- function(datos, asignacion, K){
  nuevos_centroides <- matrix(0, nrow=K, ncol=2)

  for(k in 1:K){
    # Extraemos todos los puntos asignados al cluster k
    puntos <- datos[asignacion == k, , drop=FALSE]

    # Calculamos manualmente el centroide:
    # media de la columna 1 (Velocidad)
    # media de la columna 2 (Temp)
    nuevos_centroides[k,1] <- media(puntos[,1])
    nuevos_centroides[k,2] <- media(puntos[,2])
  }

  return(nuevos_centroides)
}


### =============================
### ITERACIONES DEL ALGORITMO K-MEANS
### =============================

iter <- 1
repeat{
  cat("\n--- Iteración", iter, "---\n")

  # Paso A.2 / B.2: asignación de clusters
  asignacion_nueva <- asignar_clusters(datos, centroides)
  print(asignacion_nueva)

  # Criterio de parada: si no cambia la clasificación, ha convergido
  if(iter > 1 && all(asignacion_nueva == asignacion_ant)){
    cat("\n>>> La clasificación ha convergido.\n")
    break
  }

  # Paso B.1: recalcular centroides
  centroides <- recalcular_centroides(datos, asignacion_nueva, K)
  print(centroides)

  asignacion_ant <- asignacion_nueva
  iter <- iter + 1
}


### =============================
### RESULTADOS FINALES
### =============================

cat("\n\n=========================\n")
cat("   CLUSTER FINAL\n")
cat("=========================\n")
print(asignacion_nueva)

cat("\nCentroides finales:\n")
print(centroides)
