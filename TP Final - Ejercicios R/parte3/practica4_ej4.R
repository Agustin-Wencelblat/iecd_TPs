# IECD - Practica 4 (Bootstrap) - Ejercicio 4
# Esta equilibrado el dado? Bootstrap no parametrico para una proporcion


# uso esto en vez de scan
dado <- c(2,2,4,6,1,3,1,3,2,4,4,4,4,4,6,3,3,4,1,2,1,6,3,2,3,4,1,1,5,4,1,4,6,
          4,1,2,1,5,4,3,3,1,3,1,6,5,1,3,2,3,6,2,4,2,6,6,5,2,4,4,1,4,3,1,2,1,
          6,1,1,3,1,6,6,1,2,6,1,1,4,5,4,1,5,2,2,1,6,6,1,2,1,3,1,3,3,4,3,3,3,5)
n <- length(dado)      # 100
alpha <- 0.05
B <- 5000

# Funcion general: hace (i)-(v) para el estimador "proporcion de datos que cumplen cond"
analizar <- function(datos, estimador, valor_equilibrado, titulo) {
  theta_obs <- estimador(datos)

  # (i) una muestra bootstrap: n datos tomados con reposicion de los observados
  muestra_boot <- sample(datos, size = length(datos), replace = TRUE)
  estimador(muestra_boot)

  # (ii) B muestras bootstrap
  titas.boot <- replicate(B, estimador(sample(datos, size = length(datos), replace = TRUE)))

  # (iii) histograma
  hist(titas.boot, freq = FALSE, breaks = 25, col = "lightblue", border = "white",
       main = titulo, xlab = expression(hat(theta)^"*"),
       xlim = range(c(titas.boot, valor_equilibrado)))   # que se vea también el valor teórico
  abline(v = theta_obs, col = "red", lwd = 2)
  abline(v = valor_equilibrado, col = "black", lwd = 2, lty = 2)

  # (iv) error estandar bootstrap
  se_boot <- sd(titas.boot)

  # (v) intervalos de nivel 0.95
  z <- qnorm(1 - alpha / 2)
  ic_normal    <- theta_obs + c(-1, 1) * z * se_boot
  ic_percentil <- quantile(titas.boot, c(alpha / 2, 1 - alpha / 2))

  list(theta_obs = theta_obs, se_boot = se_boot,
       ic_normal = ic_normal, ic_percentil = ic_percentil,
       valor_si_equilibrado = valor_equilibrado)
}

set.seed(4)
par(mfrow = c(1, 2))
# a) theta = P(resultado par); si el dado es equilibrado vale 3/6 = 1/2
res_par  <- analizar(dado, function(d) mean(d %% 2 == 0), 1/2, "P(par)")
# b) theta = P(resultado = 5); si el dado es equilibrado vale 1/6
res_cinco <- analizar(dado, function(d) mean(d == 5), 1/6, "P(sale 5)")
par(mfrow = c(1, 1))

res_par
res_cinco
