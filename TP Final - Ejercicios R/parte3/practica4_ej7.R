# IECD - Practica 4 (Bootstrap) - Ejercicio 7
# Bootstrap de la mediana (y de la media) de Price en MustangPrice

MustangPrice <- read.csv("MustangPrice.csv")    # o: library(Lock5Data); data("MustangPrice")
price <- MustangPrice$Price
n <- length(price)
B <- 5000

# a) medianas bootstrap
set.seed(7)
medianas_boot <- replicate(B, median(sample(price, size = n, replace = TRUE)))

# b) histograma
hist(medianas_boot, breaks = 60, col = "lightgreen", border = "white",
     main = "Medianas bootstrap de Price", xlab = "mediana")
abline(v = median(price), col = "red", lwd = 2)

# cuantos valores distintos toma la mediana bootstrap?
length(unique(medianas_boot))
sort(table(medianas_boot), decreasing = TRUE)[1:6] / B
sort(price)

# c) lo mismo con la media
medias_boot <- replicate(B, mean(sample(price, size = n, replace = TRUE)))
hist(medias_boot, breaks = 40, freq = FALSE, col = "lightblue", border = "white",
     main = "Medias bootstrap de Price", xlab = "media")
abline(v = mean(price), col = "red", lwd = 2)
length(unique(medias_boot))

# Intervalos de nivel 0.95 para la media con ambos métodos
mean(price) + c(-1, 1) * qnorm(0.975) * sd(medias_boot) # normal
quantile(medias_boot, c(0.025, 0.975)) # percentil
