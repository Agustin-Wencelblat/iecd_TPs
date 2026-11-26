# IECD - Practica 5 (Tests) - Ejercicio 21 d)
# Test phi (se solapan los IC?) vs. test t de dos muestras (psi)

set.seed(21)
n <- 15; m <- 15
mu1   <- 1
sigma <- 5                     # N(mu, 25): desvIo 5
alpha <- 0.05
Nrep  <- 10000
mu2s  <- c(1, 1.1, 1.5, 2, 3, 4, 5, 6)

# IC t de nivel 1 - alpha para la media de cada muestra (normal, varianza desconocida)
ic_t <- function(media, desvio, n) {
  media + outer(desvio / sqrt(n), c(-1, 1)) * qt(1 - alpha / 2, df = n - 1)
}

potencia <- t(sapply(mu2s, function(mu2) {
  X <- matrix(rnorm(Nrep * n, mu1, sigma), nrow = Nrep)
  Y <- matrix(rnorm(Nrep * m, mu2, sigma), nrow = Nrep)
  mx <- rowMeans(X); my <- rowMeans(Y)
  sx <- apply(X, 1, sd); sy <- apply(Y, 1, sd)

  # phi: rechaza si IC1 y IC2 no se cortan
  ic1 <- ic_t(mx, sx, n); ic2 <- ic_t(my, sy, m)
  phi <- (ic2[, 1] > ic1[, 2]) | (ic2[, 2] < ic1[, 1])

  # psi: test t con varianzas iguales (S_p^2), t_{n+m-2}
  sp2 <- ((n - 1) * sx^2 + (m - 1) * sy^2) / (n + m - 2)
  tobs <- (my - mx) / sqrt(sp2 * (1 / n + 1 / m))
  psi <- abs(tobs) > qt(1 - alpha / 2, df = n + m - 2)

  c(mu2 = mu2, phi = mean(phi), psi = mean(psi))
}))
potencia

# Nivel teOrico de phi si las varianzas fueran conocidas e iguales y n = m:
# rechaza si |Xbar - Ybar| > 2 z sigma/sqrt(n)  =>  P(|N(0,1)| > sqrt(2) z)
2 * (1 - pnorm(sqrt(2) * qnorm(0.975)))
