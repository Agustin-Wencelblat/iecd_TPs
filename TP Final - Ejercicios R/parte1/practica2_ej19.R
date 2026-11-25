# =============================================================================
# IECD - Práctica 2 (versión corregida 2c2026) - Ejercicio 19 c)
# U(0, theta): EMV = max(X) vs. estimador de momentos = 2 * mean(X)
# Aproximar P(|theta_hat - theta0| < eps) por simulación y por la distribución límite
# =============================================================================

set.seed(2026)
theta0 <- 1
eps    <- 0.01
K      <- 1000
ns     <- c(10, 100, 500, 1000, 2000, 5000, 10000, 100000)

res <- data.frame(n = ns, sim_emv = NA, sim_mom = NA)

for (j in seq_along(ns)) {
  n <- ns[j]
  p_emv <- logical(K)
  p_mom <- logical(K)
  for (k in 1:K) {
    x <- runif(n, min = 0, max = theta0)          # A. muestra U(0, theta0)
    theta_emv <- max(x)
    theta_mom <- 2 * mean(x)
    p_emv[k] <- abs(theta_emv - theta0) < eps     # B. ¿cayó a menos de eps?
    p_mom[k] <- abs(theta_mom - theta0) < eps
  }
  res$sim_emv[j] <- mean(p_emv)                   # D. frecuencia relativa
  res$sim_mom[j] <- mean(p_mom)
}

# iii. Aproximaciones con las distribuciones límite
#  EMV: n (theta - X_(n))  ->  E(1/theta)  =>  P(|.| < eps) ~ 1 - exp(-n eps / theta)
#  MOM: sqrt(n)(2 Xbar - theta) -> N(0, theta^2/3)
#       =>  P(|.| < eps) ~ 2 Phi(eps sqrt(3n) / theta) - 1
res$asint_emv  <- pexp(ns * eps, rate = 1 / theta0)
res$asint_mom  <- 2 * pnorm(eps * sqrt(3 * ns) / theta0) - 1
# Para el EMV también se conoce la probabilidad exacta: 1 - (1 - eps/theta)^n
res$exacta_emv <- 1 - (1 - eps / theta0)^ns

print(round(res, 4))

# Gráfico: probabilidades simuladas (puntos) y aproximaciones asintóticas (líneas)
plot(ns, res$sim_emv, log = "x", pch = 19, ylim = c(0, 1),
     xlab = "n (escala log)", ylab = expression(P(abs(hat(theta)[n] - theta[0]) < epsilon)),
     main = "U(0,1), epsilon = 0.01")
lines(ns, res$asint_emv, lwd = 2)
points(ns, res$sim_mom, pch = 17, col = "red")
lines(ns, res$asint_mom, lwd = 2, col = "red")
legend("bottomright", bty = "n",
       legend = c("EMV simulado", "EMV: aprox. exponencial",
                  "Momentos simulado", "Momentos: aprox. normal"),
       pch = c(19, NA, 17, NA), lty = c(NA, 1, NA, 1), lwd = 2,
       col = c("black", "black", "red", "red"))
