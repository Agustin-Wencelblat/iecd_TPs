# IECD - Practica 3 - Ejercicio 11
# IC de nivel asintótico 0.95 para p en una B(1, p): Wald vs. cuadrática (Wilson)


set.seed(2025)
alpha <- 0.05
z     <- qnorm(1 - alpha / 2)      
k     <- 2000

# Los dos metodos: reciben p_hat = Xbar y n, devuelven los extremos 
ic_metodo1 <- function(p_hat, n) {
  # Se reemplaza p por Xbar en la varianza p(1-p)/n
  se <- sqrt(p_hat * (1 - p_hat) / n)
  cbind(inf = p_hat - z * se, sup = p_hat + z * se)
}

ic_metodo2 <- function(p_hat, n) {
  # Se despeja p de (Xbar - p)^2 <= z^2 p(1-p)/n  (raíces de una cuadr)
  centro <- (p_hat + z^2 / (2 * n)) / (1 + z^2 / n)
  semi   <- z * sqrt(p_hat * (1 - p_hat) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  cbind(inf = centro - semi, sup = centro + semi)
}

simular <- function(n, p) {
  muestras <- matrix(rbinom(k * n, size = 1, prob = p), nrow = k)  
  p_hat <- rowMeans(muestras)

  resultados <- list()
  for (m in 1:2) {
    ic <- if (m == 1) ic_metodo1(p_hat, n) else ic_metodo2(p_hat, n)
    resultados[[m]] <- data.frame(
      metodo   = m,
      inf      = ic[, "inf"],
      sup      = ic[, "sup"],
      longitud = ic[, "sup"] - ic[, "inf"],
      cubre    = as.integer(ic[, "inf"] <= p & p <= ic[, "sup"]) 
    )
  }
  do.call(rbind, resultados)
}

# Todas las combinaciones
grilla  <- expand.grid(n = c(20, 50, 100), p = c(0.10, 0.50))
tabla   <- data.frame()
guardar <- list()     

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

