# ==========================================================
# CREACIÓN DEL CONJUNTO DE DATOS
# ==========================================================
datos <- data.frame(
  Velocidad = c(10,8,13,9,11,14,6,4,12,7,5),
  Temperatura = c(7.46,6.77,12.74,7.11,7.81,8.84,6.08,5.39,8.15,6.42,5.73)
)

# ==========================================================
# FUNCIONES AUXILIARES BÁSICAS (del Ejercicio 2.1)
# ==========================================================

# Contar elementos manualmente
contar_elementos <- function(x) {
  n <- 0
  for (._ in x) n <- n + 1
  n
}

# Suma manual
mi_suma <- function(x) {
  total <- 0
  for (valor in x) total <- total + valor
  total
}

# Media manual
media_manual <- function(x) {
  suma <- mi_suma(x)
  n <- contar_elementos(x)
  suma / n
}

# Varianza manual
varianza_manual <- function(x) {
  n <- contar_elementos(x)
  media <- media_manual(x)
  suma <- 0
  for (v in x) suma <- suma + (v - media)^2
  suma / n
}

# Desviación típica manual
desviacion_manual <- function(x) {
  sqrt(varianza_manual(x))
}

# ==========================================================
# NUEVAS FUNCIONES DE DETECCIÓN DE OUTLIERS (sin list, length, etc.)
# ==========================================================

# Método IQR manual
detectar_outliers_iqr <- function(x) {
  resumen <- fivenum(x)
  Q1 <- resumen[2]
  Q3 <- resumen[4]
  IQRv <- Q3 - Q1
  lim_inf <- Q1 - 1.5 * IQRv
  lim_sup <- Q3 + 1.5 * IQRv
  
  cat("\n--- DETECCIÓN OUTLIERS IQR ---\n")
  cat("Límite inferior:", lim_inf, "\n")
  cat("Límite superior:", lim_sup, "\n")
  
  i <- 1
  hay_outliers <- FALSE
  for (v in x) {
    if (v < lim_inf || v > lim_sup) {
      cat("Índice:", i, "→ Valor =", v, "es un outlier\n")
      hay_outliers <- TRUE
    }
    i <- i + 1
  }
  if (!hay_outliers) cat("No se detectaron outliers.\n")
}

# Método de la desviación típica manual
detectar_outliers_sd <- function(x, d = 2) {
  media <- media_manual(x)
  desv <- desviacion_manual(x)
  lim_inf <- media - d * desv
  lim_sup <- media + d * desv
  
  cat("\n--- DETECCIÓN OUTLIERS DESVIACIÓN TÍPICA ---\n")
  cat("Media:", media, "\n")
  cat("Desviación típica:", desv, "\n")
  cat("Límite inferior:", lim_inf, "\n")
  cat("Límite superior:", lim_sup, "\n")
  
  i <- 1
  hay_outliers <- FALSE
  for (valor in x) {
    if (valor < lim_inf || valor > lim_sup) {
      cat("Índice:", i, "→ Valor =", valor, "es un outlier\n")
      hay_outliers <- TRUE
    }
    i <- i + 1
  }
  if (!hay_outliers) cat("No se detectaron outliers.\n")
}

# Método de regresión manual con detección por residuos
detectar_outliers_regresion <- function(x, y, d = 2) {
  n <- contar_elementos(x)
  media_x <- media_manual(x)
  media_y <- media_manual(y)
  
  # Calcular pendiente y ordenada al origen
  num <- 0
  den <- 0
  for (i in seq(n)) {
    num <- num + (x[i] - media_x) * (y[i] - media_y)
    den <- den + (x[i] - media_x)^2
  }
  b1 <- num / den
  b0 <- media_y - b1 * media_x
  
  # Calcular residuos
  residuos <- numeric(n)
  for (i in seq(n)) {
    y_est <- b0 + b1 * x[i]
    residuos[i] <- y[i] - y_est
  }
  
  # Calcular error estándar de residuos
  suma_res <- 0
  for (r in residuos) suma_res <- suma_res + r^2
  error_est <- sqrt(suma_res / n)
  
  cat("\n--- DETECCIÓN OUTLIERS REGRESIÓN ---\n")
  cat("Ecuación: y =", round(b0, 4), "+", round(b1, 4), "* x\n")
  cat("Error estándar de residuos:", round(error_est, 4), "\n")
  
  i <- 1
  hay_outliers <- FALSE
  for (r in residuos) {
    if (abs(r) > d * error_est) {
      cat("Índice:", i, "→ Residuo =", round(r, 4), "es un outlier\n")
      hay_outliers <- TRUE
    }
    i <- i + 1
  }
  if (!hay_outliers) cat("No se detectaron outliers en los residuos.\n")
}

# ==========================================================
# APLICACIÓN DE LOS MÉTODOS
# ==========================================================

detectar_outliers_iqr(datos$Velocidad)
detectar_outliers_sd(datos$Temperatura)
detectar_outliers_regresion(datos$Velocidad, datos$Temperatura)
