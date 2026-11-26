# IECD - Practica 5 (Tests) - Ejercicio 9 e) y f)
# Bernoulli, H0: p = p0 vs H1: p != p0. Potencia de los tests W y T.

n     <- 50
p0    <- 0.3
alpha <- 0.05
z     <- qnorm(1 - alpha / 2)

# e) Potencia aproximada (aproximacion normal: Xbar ~ N(p, p(1-p)/n))
# Test W: rechaza si |Xbar - p0| > z sqrt(p0(1-p0)/n)  -> región fija en Xbar
pot_W <- function(p) {
  c0 <- z * sqrt(p0 * (1 - p0) / n)
  sp <- sqrt(p * (1 - p) / n)
  1 - pnorm((p0 + c0 - p) / sp) + pnorm((p0 - c0 - p) / sp)
}

# Test T: rechaza si (Xbar - p0)^2 > z^2 Xbar(1-Xbar)/n.
# No rechaza si Xbar esta entre las raíces de (1 + z^2/n) x^2 - (2 p0 + z^2/n) x + p0^2 = 0
raices_T <- function() {
  a <- 1 + z^2 / n; b <- -(2 * p0 + z^2 / n); c <- p0^2
  sort((-b + c(-1, 1) * sqrt(b^2 - 4 * a * c)) / (2 * a))
}
pot_T <- function(p) {
  r  <- raices_T()
  sp <- sqrt(p * (1 - p) / n)
  1 - (pnorm((r[2] - p) / sp) - pnorm((r[1] - p) / sp))
}

ps <- seq(0.01, 0.99, by = 0.005)
plot(ps, pot_W(ps), type = "l", lwd = 2, ylim = c(0, 1),
     xlab = "p", ylab = expression(pi(p)),
     main = "Potencia aproximada, n = 50, p0 = 0.3")
lines(ps, pot_T(ps), lwd = 2, col = "red", lty = 2)
abline(h = alpha, lty = 3); abline(v = p0, lty = 3)
legend("bottomright", bty = "n", lwd = 2, lty = c(1, 2), col = c("black", "red"),
       legend = c("Test W (varianza bajo H0)", "Test T (varianza estimada)"))

c(nivel_aprox_W = pot_W(p0), nivel_aprox_T = pot_T(p0))
raices_T()       # región de aceptación del test T en la escala de Xbar
p0 + c(-1, 1) * z * sqrt(p0 * (1 - p0) / n)   # ídem para el test W

# f) Potencia por simulación
set.seed(9)
Nrep  <- 10000
p_sim <- c(0.03, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.97)

potencia_sim <- t(sapply(p_sim, function(p) {
  xbar <- rbinom(Nrep, size = n, prob = p) / n # Xbar de Nrep muestras de tamaño n
  W <- sqrt(n) * (xbar - p0) / sqrt(p0 * (1 - p0))
  T <- sqrt(n) * (xbar - p0) / sqrt(xbar * (1 - xbar)) # si Xbar = 0 ó 1 da +-Inf: rechaza
  c(p = p, W = mean(abs(W) > z), T = mean(abs(T) > z))
}))
potencia_sim

plot(potencia_sim[, "p"], potencia_sim[, "W"], type = "b", pch = 19, lwd = 2,
     ylim = c(0, 1), xlab = "p", ylab = expression(pi(p)),
     main = "Potencia simulada (10000 réplicas)")
lines(potencia_sim[, "p"], potencia_sim[, "T"], type = "b", pch = 17, lwd = 2,
      col = "red", lty = 2)
abline(h = alpha, lty = 3)
legend("bottomright", bty = "n", lwd = 2, lty = c(1, 2), pch = c(19, 17),
       col = c("black", "red"), legend = c("Test W", "Test T"))

# Chequeo: potencia exacta con la binomial
pot_exacta <- function(p, stat) {
  x <- 0:n; xb <- x / n
  s <- if (stat == "W") sqrt(n) * (xb - p0) / sqrt(p0 * (1 - p0))
       else             sqrt(n) * (xb - p0) / sqrt(xb * (1 - xb))
  sum(dbinom(x, n, p)[abs(s) > z])
}
cbind(p = p_sim,
      W_exacta = sapply(p_sim, pot_exacta, stat = "W"),
      T_exacta = sapply(p_sim, pot_exacta, stat = "T"))
