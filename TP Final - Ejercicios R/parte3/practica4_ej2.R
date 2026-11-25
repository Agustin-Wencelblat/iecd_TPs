# IECD - Practica 4 (Bootstrap) - Ejercicio 2
# Poisson: estimadores de momentos T1 (orden 1) y T2 (orden 2), bootstrap paramétrico

x <- c(500, 488, 426, 510, 450, 368, 508, 514, 426,
       476, 512, 526, 444, 524, 236)
n <- length(x)

# a) Estimadores de momentos 
# T1: E(X) = lambda               =>  lambda = media
# T2: E(X^2) = lambda + lambda^2  =>  lambda = (-1 + sqrt(1 + 4 m2)) / 2
T1 <- function(x) mean(x)
T2 <- function(x) (-1 + sqrt(1 + 4 * mean(x^2))) / 2

T1_obs <- T1(x)
T2_obs <- T2(x)
c(T1_obs = T1_obs, T2_obs = T2_obs)

# b) Bootstrap parametrico para T1 
set.seed(1)
B <- 1000

# 1) una muestra bootstrap: 15 datos Poisson con lambda = T1_obs
x_boot <- rpois(n, lambda = T1_obs)
T1(x_boot)

# 2) repito B veces
T1_boot <- replicate(B, T1(rpois(n, lambda = T1_obs)))

# 3) histograma
hist(T1_boot, freq = FALSE, breaks = 30, col = "lightblue", border = "white",
     main = "Bootstrap paramétrico de T1", xlab = expression(T[1]^"*"))
abline(v = T1_obs, col = "red", lwd = 2)

# 4) varianza estimada de T1 y comparamos con la teorica lambda/n
c(var_boot = var(T1_boot), var_teorica_plugin = T1_obs / n)

# c) Lo mismo para T2 (ahora generando con lambda = T2_obs) 
T2_boot <- replicate(B, T2(rpois(n, lambda = T2_obs)))
hist(T2_boot, freq = FALSE, breaks = 30, col = "lightgreen", border = "white",
     main = "Bootstrap paramétrico de T2", xlab = expression(T[2]^"*"))
abline(v = T2_obs, col = "red", lwd = 2)
c(var_boot_T2 = var(T2_boot))

# Comparacion
c(var_T1 = var(T1_boot), var_T2 = var(T2_boot), cociente = var(T2_boot) / var(T1_boot))
