# =============================================================================
# IECD - Práctica 5 (Tests) - Ejercicio 12
# "Espiar" los datos a mitad de camino: nivel y potencia del procedimiento
# =============================================================================

alpha <- 0.05
z     <- qnorm(1 - alpha)          # z_{0.05} = 1.645 (cola superior)
Nrep  <- 100000

# Procedimiento del científico: rechaza si T_20 > z, o si no y T_30 > z
procedimiento <- function(mu, Nrep) {
  X   <- matrix(rnorm(Nrep * 30, mean = mu, sd = 1), nrow = Nrep)  # 30 datos por réplica
  T20 <- sqrt(20) * rowMeans(X[, 1:20])
  T30 <- sqrt(30) * rowMeans(X)
  c(procedimiento = mean(T20 > z | T30 > z),   # lo que hace el científico
    solo_n30      = mean(T30 > z))             # el test planeado originalmente
}

# a) Nivel: simulamos bajo H0 (mu = 0)
set.seed(12)
procedimiento(0, Nrep)

# Valor exacto: (T20, T30) es normal bivariada con correlación sqrt(20/30)
rho <- sqrt(20 / 30)
# P(T20 > z o T30 > z) = 1 - P(T20 <= z, T30 <= z); lo calculamos integrando en T20
f <- function(t) dnorm(t) * pnorm((z - rho * t) / sqrt(1 - rho^2))
1 - integrate(f, -Inf, z)$value

# b) Función de potencia en mu = 0.5, 1, 2, 5, 10
mus <- c(0.5, 1, 2, 5, 10)
pot <- t(sapply(mus, procedimiento, Nrep = Nrep))
cbind(mu = mus, pot,
      teorica_n30 = 1 - pnorm(z - sqrt(30) * mus))  # potencia exacta del test con n = 30
