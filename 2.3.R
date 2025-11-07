# Crear los datos
datos <- data.frame(
  Velocidad = c(10,8,13,9,11,14,6,4,12,7,5),
  Temperatura = c(7.46,6.77,12.74,7.11,7.81,8.84,6.08,5.39,8.15,6.42,5.73)
)

# Función para calcular la media del ejer 2.1
media_manual <- function(x) {
  # Se incializa la suma a cero
  suma <- 0
  # Se cuenta el número de elementos
  n <- contar_elementos(x)
  # Se suma cada valor
  for (v in x) suma <- suma + v
  # Se retorna la media
  suma / n
}

# Función ejer 2.1
contar_elementos <- function(x) {
  # Se inicializa el contador
  n <- 0
  # Se incrementa por cada elemento del vector
  for (._ in x) n <- n + 1
  # Se retorna el total
  n
}

mi_suma <- function(x) {
  total <- 0
  for (valor in x) total <- total + valor
  total
}


### --- 1. Medidas de ordenación (Velocidad) ---
resumen_vel <- fivenum(datos$Velocidad)
names(resumen_vel) <- c("Mínimo", "Q1", "Mediana", "Q3", "Máximo")
print(resumen_vel)

IQRv <- resumen_vel["Q3"] - resumen_vel["Q1"]
lim_inf_v <- resumen_vel["Q1"] - 1.5 * IQRv
lim_sup_v <- resumen_vel["Q3"] + 1.5 * IQRv

cat("\n--- OUTLIERS en VELOCIDAD ---\n")
i <- 1
for (v in datos$Velocidad) {
  if (v < lim_inf_v || v > lim_sup_v) {
    cat("Índice:", i, "→ Velocidad =", v, "es un outlier\n")
  }
  i <- i + 1
}
if (all(datos$Velocidad >= lim_inf_v & datos$Velocidad <= lim_sup_v)) {
  cat("No se detectaron outliers en velocidad.\n")
}
### --- 2. Medidas de dispersión (Temperatura) ---
n <- contar_elementos(datos$Temperatura)
media_temp <- media_manual(datos$Temperatura)

# cálculo manual de la varianza poblacional
var_temp <- 0
for (t in datos$Temperatura) {
  var_temp <- var_temp + (t - media_temp)^2
}
var_temp <- var_temp / n
desv_temp <- sqrt(var_temp)


lim_inf_t <- media_temp - 2 * desv_temp
lim_sup_t <- media_temp + 2 * desv_temp

cat("\n--- OUTLIERS en TEMPERATURA ---\n")
i <- 1
for (t in datos$Temperatura) {
  if (t < lim_inf_t || t > lim_sup_t) {
    cat("Índice:", i, "→ Temperatura =", t, "es un outlier\n")
  }
  i <- i + 1
}

### --- 3. Regresión manual y detección de outliers por residuos ---

x <- datos$Velocidad
y <- datos$Temperatura
media_x <- media_manual(x)
media_y <- media_manual(y)

# Pendiente (b1) manual
num <- 0
den <- 0
for (i in seq(contar_elementos(x))) {
  num <- num + (x[i] - media_x) * (y[i] - media_y)
  den <- den + (x[i] - media_x)^2
}
b1 <- num / den
b0 <- media_y - b1 * media_x

cat("\nEcuación de regresión manual:\n")
cat("Temperatura = ", round(b0, 4), " + ", round(b1, 4), " * Velocidad\n")

# Calcular residuos y error estándar
residuos <- numeric(n)
for (i in seq(n)) {
  y_est <- b0 + b1 * x[i]
  residuos[i] <- y[i] - y_est
}

# Error estándar de residuos manual
suma_res <- 0
for (r in residuos) suma_res <- suma_res + r^2
error_est <- sqrt(suma_res / n)
cat("\nError estándar de los residuos:", round(error_est, 4), "\n")

# Detección de outliers de regresión
cat("\n--- OUTLIERS en REGRESIÓN ---\n")
i <- 1
for (r in residuos) {
  if (abs(r) > 2 * error_est) {
    cat("Índice:", i, "→ Residuo =", round(r, 4), "es un outlier\n")
  }
  i <- i + 1
}
