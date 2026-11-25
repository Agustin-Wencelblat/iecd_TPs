# =============================================================================
# IECD - Práctica 3 (2025) - Ejercicio 11
# IC de nivel asintótico 0.95 para p en una B(1, p): Wald vs. cuadrática (Wilson)
# =============================================================================

set.seed(2025)
alpha <- 0.05
z     <- qnorm(1 - alpha / 2)      # z_{alpha/2} (cola superior) = 1.96
k     <- 2000

# --- Los dos métodos: reciben p_hat = Xbar y n, devuelven los extremos -------
ic_metodo1 <- function(p_hat, n) {
  # Se reemplaza p por Xbar en la varianza p(1-p)/n
  se <- sqrt(p_hat * (1 - p_hat) / n)
  cbind(inf = p_hat - z * se, sup = p_hat + z * se)
}

ic_metodo2 <- function(p_hat, n) {
  # Se despeja p de (Xbar - p)^2 <= z^2 p(1-p)/n  (raíces de una cuadrática)
  centro <- (p_hat + z^2 / (2 * n)) / (1 + z^2 / n)
  semi   <- z * sqrt(p_hat * (1 - p_hat) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  cbind(inf = centro - semi, sup = centro + semi)
}

# --- Simulación para una combinación (n, p) ------------------------------------
simular <- function(n, p) {
  muestras <- matrix(rbinom(k * n, size = 1, prob = p), nrow = k)  # k muestras de tamaño n
  p_hat <- rowMeans(muestras)

  resultados <- list()
  for (m in 1:2) {
    ic <- if (m == 1) ic_metodo1(p_hat, n) else ic_metodo2(p_hat, n)
    resultados[[m]] <- data.frame(
      metodo   = m,
      inf      = ic[, "inf"],
      sup      = ic[, "sup"],
      longitud = ic[, "sup"] - ic[, "inf"],
      cubre    = as.integer(ic[, "inf"] <= p & p <= ic[, "sup"])   # 1 si contiene a p
    )
  }
  do.call(rbind, resultados)
}

# --- Todas las combinaciones -----------------------------------------------------
grilla  <- expand.grid(n = c(20, 50, 100), p = c(0.10, 0.50))
tabla   <- data.frame()
guardar <- list()     # acá quedan los 2000 intervalos de cada combinación

for (i in seq_len(nrow(grilla))) {
  n <- grilla$n[i]; p <- grilla$p[i]
  res <- simular(n, p)
  guardar[[paste0("n", n, "_p", p)]] <- res
  for (m in 1:2) {
    r <- res[res$metodo == m, ]
    tabla <- rbind(tabla, data.frame(
      n = n, p = p, metodo = m,
      long_esperada = mean(r$longitud),   # a)
      cubrimiento   = mean(r$cubre)       # b)
    ))
  }
}

print(tabla[order(tabla$p, tabla$n, tabla$metodo), ], row.names = FALSE, digits = 4)

# --- Extra: ¿por qué falla el método 1 con p = 0.1? ------------------------------
# Proporción de muestras con Xbar = 0 (el IC del método 1 queda [0, 0])
sapply(guardar[c("n20_p0.1", "n50_p0.1", "n100_p0.1")], function(res) {
  r <- res[res$metodo == 1, ]
  mean(r$longitud == 0)
})
# Proporción de IC del método 1 con extremo inferior negativo
sapply(guardar[c("n20_p0.1", "n50_p0.1", "n100_p0.1")], function(res) {
  r <- res[res$metodo == 1, ]
  mean(r$inf < 0)
})
