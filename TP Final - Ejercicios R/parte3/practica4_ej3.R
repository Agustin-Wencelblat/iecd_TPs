# IECD - Practica 4 (Bootstrap) - Ejercicio 3
# Error estándar bootstrap de la media y de la mediana (datos1.txt)

x <- read.table("datos1.txt", header = TRUE)$x
n <- length(x)

# (a) y (b): estimaciones del valor central
c(media = mean(x), mediana = median(x))

# Esquema bootstrap
set.seed(3)
B <- 1000
medias_boot   <- numeric(B)
medianas_boot <- numeric(B)
for (b in 1:B) {
  x_boot <- sample(x, size = n, replace = TRUE) # a) remuestra del mismo tamano
  medias_boot[b]   <- mean(x_boot) # b) medida de posicion
  medianas_boot[b] <- median(x_boot)
} # c) B repeticiones

# d) error estandar bootstrap de cada estimador
se_media   <- sd(medias_boot)
se_mediana <- sd(medianas_boot)
c(se_media = se_media, se_mediana = se_mediana, cociente = se_mediana / se_media)

sd(x) / sqrt(n)  # se(media) estimado por la formula: S_n / sqrt(n)
sqrt(pi / 2) * sd(x) / sqrt(n)  # se(mediana) si los datos fueran normales

par(mfrow = c(1, 2))
hist(medias_boot,   breaks = 30, col = "lightblue",  border = "white",
     main = "Medias bootstrap",   xlab = "media")
hist(medianas_boot, breaks = 30, col = "lightgreen", border = "white",
     main = "Medianas bootstrap", xlab = "mediana")
par(mfrow = c(1, 1))
