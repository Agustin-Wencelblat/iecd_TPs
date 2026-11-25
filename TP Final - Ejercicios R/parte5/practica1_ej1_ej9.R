# =============================================================================
# IECD - Práctica 1 - Ejercicios 1 y 9 (Debernardi.csv, cáncer de páncreas)
# diagnosis: 1 = control sano, 2 = enfermedad benigna del páncreas/hepatobiliar,
#            3 = adenocarcinoma ductal de páncreas (cáncer)
# =============================================================================

datos <- read.csv("Debernardi.csv")
datos$diagnosis <- factor(datos$diagnosis)
colores <- c("1" = "#1b9e77", "2" = "#d95f02", "3" = "#7570b3")

# -----------------------------------------------------------------------------
# Ejercicio 1
# -----------------------------------------------------------------------------
# a) tabla de frecuencias absolutas y relativas
tabla <- table(datos$diagnosis)
cbind(frecuencia = tabla, frec_relativa = round(prop.table(tabla), 4))

# b) gráfico de barras
barplot(prop.table(tabla), col = colores, ylim = c(0, 0.4),
        names.arg = c("1: control", "2: benigna", "3: cáncer"),
        ylab = "Frecuencia relativa", main = "Distribución de diagnosis")

# -----------------------------------------------------------------------------
# Ejercicio 9
# -----------------------------------------------------------------------------
lyve <- split(datos$LYVE1, datos$diagnosis)
sapply(lyve, summary)

# a) histogramas por nivel de diagnosis (misma escala para poder comparar)
par(mfrow = c(1, 3))
for (g in names(lyve)) {
  hist(lyve[[g]], breaks = seq(0, 24, by = 1), freq = FALSE, col = colores[g],
       border = "white", ylim = c(0, 0.6),
       main = paste("diagnosis =", g), xlab = "LYVE1")
}
par(mfrow = c(1, 1))

# b) funciones de distribución empíricas superpuestas
plot(ecdf(lyve[["1"]]), do.points = FALSE, verticals = TRUE, col = colores["1"], lwd = 2,
     xlim = c(0, 24), main = "F empírica de LYVE1 según diagnosis",
     xlab = "LYVE1", ylab = expression(hat(F)[n](x)))
plot(ecdf(lyve[["2"]]), do.points = FALSE, verticals = TRUE, col = colores["2"], lwd = 2, add = TRUE)
plot(ecdf(lyve[["3"]]), do.points = FALSE, verticals = TRUE, col = colores["3"], lwd = 2, add = TRUE)
legend("right", inset = 0.03, bg = "white", lwd = 2, col = colores,
       legend = c("1: control", "2: benigna", "3: cáncer"))

# c) boxplots paralelos según diagnosis y sexo
boxplot(LYVE1 ~ sex + diagnosis, data = datos,
        col = rep(colores, each = 2), at = c(1, 2, 4, 5, 7, 8),
        names = paste(c("F", "M"), rep(1:3, each = 2)),
        xlab = "sexo y diagnosis", ylab = "LYVE1",
        main = "LYVE1 según diagnosis y sexo")
aggregate(LYVE1 ~ sex + diagnosis, data = datos, FUN = median)

# d) densidades estimadas superpuestas
dens <- lapply(lyve, density)
plot(dens[["1"]], col = colores["1"], lwd = 2, xlim = c(-2, 24),
     ylim = c(0, max(sapply(dens, function(d) max(d$y)))),
     main = "Densidades estimadas de LYVE1", xlab = "LYVE1")
lines(dens[["2"]], col = colores["2"], lwd = 2)
lines(dens[["3"]], col = colores["3"], lwd = 2)
legend("topright", bty = "n", lwd = 2, col = colores,
       legend = c("1: control", "2: benigna", "3: cáncer"))

# e) lo mismo para log(LYVE1)
loglyve <- lapply(lyve, log)
par(mfrow = c(1, 3))
for (g in names(loglyve)) {
  hist(loglyve[[g]], breaks = seq(-10, 4, by = 0.5), freq = FALSE, col = colores[g],
       border = "white", ylim = c(0, 0.6),
       main = paste("diagnosis =", g), xlab = "log(LYVE1)")
}
par(mfrow = c(1, 1))

dens_log <- lapply(loglyve, density)
plot(dens_log[["1"]], col = colores["1"], lwd = 2, xlim = c(-11, 5),
     ylim = c(0, max(sapply(dens_log, function(d) max(d$y)))),
     main = "Densidades estimadas de log(LYVE1)", xlab = "log(LYVE1)")
lines(dens_log[["2"]], col = colores["2"], lwd = 2)
lines(dens_log[["3"]], col = colores["3"], lwd = 2)
legend("topleft", bty = "n", lwd = 2, col = colores,
       legend = c("1: control", "2: benigna", "3: cáncer"))
