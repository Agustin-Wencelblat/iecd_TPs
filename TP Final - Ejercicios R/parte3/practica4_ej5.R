# =============================================================================
# IECD - Práctica 4 (Bootstrap) - Ejercicio 5
# Bootstrap de la correlación entre Price y Miles (MustangPrice)
# =============================================================================

# Con el paquete:  library(Lock5Data); data("MustangPrice")
MustangPrice <- read.csv("MustangPrice.csv")
n <- nrow(MustangPrice)

# a) Gráfico y correlación observada
plot(Price ~ Miles, data = MustangPrice, pch = 19, col = "steelblue",
     xlab = "Millas (miles)", ylab = "Precio (miles de USD)",
     main = "Mustang usados: precio vs. kilometraje")
rho_obs <- cor(MustangPrice$Price, MustangPrice$Miles)
rho_obs

# b) Mecanismo: se remuestrean AUTOS (filas), no las variables por separado.
#    Se eligen n índices al azar con reposición y se toman los pares (Miles, Price)
#    de esas filas, para conservar la relación entre las dos variables.
indices <- sample(1:n, size = n, replace = TRUE)
cor(MustangPrice$Price[indices], MustangPrice$Miles[indices])

# c) B = 5000 correlaciones bootstrap
set.seed(5)
B <- 5000
rho_boot <- replicate(B, {
  ind <- sample(1:n, size = n, replace = TRUE)
  cor(MustangPrice$Price[ind], MustangPrice$Miles[ind])
})

hist(rho_boot, breaks = 40, freq = FALSE, col = "lightblue", border = "white",
     main = "Correlaciones bootstrap", xlab = expression(hat(rho)^"*"))
abline(v = rho_obs, col = "red", lwd = 2)
c(media = mean(rho_boot), se = sd(rho_boot))

# d) Intervalo bootstrap percentil de nivel 0.95
quantile(rho_boot, c(0.025, 0.975))
