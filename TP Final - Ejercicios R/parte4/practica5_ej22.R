# IECD - Practica 5 (Tests) - Ejercicio 22 c)
# Potencia del test del signo vs el test z para la media (normal, sigma conocido)

n <- 100; a0 <- 20; sigma0 <- 4; alpha <- 0.05
z <- qnorm(1 - alpha / 2)

# Test z: rechaza si |sqrt(n)(Xbar - a0)/sigma0| >= z
pot_z <- function(mu1) {
  d <- sqrt(n) * (mu1 - a0) / sigma0
  1 - pnorm(z - d) + pnorm(-z - d)
}

# Test del signo: rechaza si |sqrt(n)(Tbar - 1/2)/(1/2)| >= z
# Bajo mu = mu1, Tbar ~ aprox N(theta1, theta1(1 - theta1)/n), theta1 = 1 - Phi((a0 - mu1)/sigma0)
pot_signo <- function(mu1) {
  th1 <- 1 - pnorm((a0 - mu1) / sigma0)
  s   <- sqrt(th1 * (1 - th1) / n)
  lim <- 1/2 + c(-1, 1) * z / (2 * sqrt(n)) # región de aceptacion tbar
  1 - pnorm((lim[2] - th1) / s) + pnorm((lim[1] - th1) / s)
}

mus <- seq(10, 30, by = 0.05)
plot(mus, pot_z(mus), type = "l", lwd = 2, ylim = c(0, 1),
     xlab = expression(mu[1]), ylab = "potencia",
     main = "n = 100, a0 = 20, sigma0 = 4")
lines(mus, pot_signo(mus), lwd = 2, col = "red", lty = 2)
abline(h = alpha, lty = 3)
legend("bottomright", bty = "n", lwd = 2, lty = c(1, 2), col = c("black", "red"),
       legend = c("test z (media)", "test del signo"))

# Algunos valores
mu_tab <- c(19, 19.5, 20, 20.5, 21, 22)
round(cbind(mu1 = mu_tab, z = pot_z(mu_tab), signo = pot_signo(mu_tab)), 4)
