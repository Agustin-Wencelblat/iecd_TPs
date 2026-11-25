# =============================================================================
# IECD - Práctica 4 (Bootstrap) - Ejercicio 8
# IC bootstrap (normal y percentil) para la mediana; cubrimiento y longitud
# =============================================================================

# a) Las dos funciones ----------------------------------------------------------------
medianas_boot <- function(x, B) {
  n <- length(x)
  # cada fila de la matriz es una muestra bootstrap
  remuestras <- matrix(sample(x, size = n * B, replace = TRUE), nrow = B)
  apply(remuestras, 1, median)
}

boot.metodo.1 <- function(x, nivel = 0.95, B = 2000) {      # bootstrap normal
  alpha <- 1 - nivel
  se <- sd(medianas_boot(x, B))
  median(x) + c(-1, 1) * qnorm(1 - alpha / 2) * se
}

boot.metodo.2 <- function(x, nivel = 0.95, B = 2000) {      # bootstrap percentil
  alpha <- 1 - nivel
  unname(quantile(medianas_boot(x, B), c(alpha / 2, 1 - alpha / 2)))
}

# b) Un intervalo con n = 30 ------------------------------------------------------------
set.seed(8)
x <- rnorm(30)
boot.metodo.1(x, nivel = 0.95, B = 2000)
boot.metodo.2(x, nivel = 0.95, B = 2000)

# c), d), e) Nrep = 1000 repeticiones para cada n y cada método ----------------------
# La mediana verdadera de una N(0,1) es 0.
simular <- function(n, metodo, Nrep = 1000, B = 2000) {
  ics <- t(replicate(Nrep, metodo(rnorm(n), nivel = 0.95, B = B)))
  c(longitud_promedio   = mean(ics[, 2] - ics[, 1]),
    cubrimiento_empirico = mean(ics[, 1] <= 0 & 0 <= ics[, 2]))
}

ns <- c(30, 50, 100, 1000)
# OJO: con n = 1000 cada método tarda varios minutos (1000 x 2000 medianas de 1000 datos).
# Para probar el código, bajar Nrep (por ejemplo a 200).
set.seed(2023)
tabla_m1 <- sapply(ns, simular, metodo = boot.metodo.1)
tabla_m2 <- sapply(ns, simular, metodo = boot.metodo.2)
colnames(tabla_m1) <- colnames(tabla_m2) <- paste0("n = ", ns)

round(tabla_m1, 4)   # Método 1: bootstrap normal
round(tabla_m2, 4)   # Método 2: bootstrap percentil

# Referencia: longitud del IC asintótico teórico de la mediana,
# sqrt(n)(mediana - 0) -> N(0, 1/(4 f(0)^2)) = N(0, pi/2)
2 * qnorm(0.975) * sqrt(pi / 2 / ns)
