# =============================================================================
# IECD - Práctica 4 (Bootstrap) - Ejercicio 6
# Diferencia de ingresos medianos entre mujeres y varones (EPH, 3er trimestre 2021)
# =============================================================================

# -----------------------------------------------------------------------------
# 0) Armado del archivo eph21_3T.csv a partir de la base individual del INDEC
#    (si tenés el eph21_3T.csv de la cátedra, saltear esta parte y leerlo directo)
#    P47T = ingreso total individual  (-9 = no respondió, 0 = sin ingresos)
#    CH04 = sexo (1 = varón, 2 = mujer)
# -----------------------------------------------------------------------------
library(readxl)                                        # install.packages("readxl") si hace falta
ind <- read_excel("usu_individual_T321_xls.xls")

ingresototal <- ind$P47T
mujer <- as.integer(ind$CH04 == 2)
cat("Personas encuestadas:", length(ingresototal), "\n")

# Excluimos ingresos nulos, no informados (-9) o faltantes
ok <- !is.na(ingresototal) & ingresototal > 0
set.seed(21)
eph <- data.frame(ingresototal = ingresototal[ok], mujer = mujer[ok])
# Ruido para deshacer el redondeo de los ingresos (la mayoría se declara redondeada
# a los mil pesos): se suma una uniforme en (-500, 500), del orden de ese redondeo.
# OJO: el ruido que usó la cátedra no está documentado; los resultados dependen de esta elección.
eph$ingresototal.conruido <- eph$ingresototal + runif(nrow(eph), -500, 500)
cat("Personas con ingreso informado y positivo:", nrow(eph), "\n")
write.csv(eph, "eph21_3T.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# Análisis
# -----------------------------------------------------------------------------
eph <- read.csv("eph21_3T.csv")
ing_m <- eph$ingresototal.conruido[eph$mujer == 1]
ing_v <- eph$ingresototal.conruido[eph$mujer == 0]
c(mujeres = length(ing_m), varones = length(ing_v))

# a) medianas y diferencia (mujeres - varones)
med_m <- median(ing_m)
med_v <- median(ing_v)
dif_obs <- med_m - med_v
c(mediana_mujeres = med_m, mediana_varones = med_v, diferencia = dif_obs)

# b) una réplica bootstrap: se remuestrea CADA grupo por separado
dif_boot_1 <- median(sample(ing_m, replace = TRUE)) - median(sample(ing_v, replace = TRUE))
dif_boot_1

# c) B = 5000 réplicas
set.seed(6)
B <- 5000
dif_boot <- replicate(B, median(sample(ing_m, replace = TRUE)) -
                         median(sample(ing_v, replace = TRUE)))

hist(dif_boot, breaks = 50, col = "lightblue", border = "white",
     main = "Diferencia de medianas bootstrap (mujeres - varones)",
     xlab = "diferencia (pesos)")
abline(v = dif_obs, col = "red", lwd = 2)
se_boot <- sd(dif_boot)
se_boot

# d) intervalos de nivel 0.95
dif_obs + c(-1, 1) * qnorm(0.975) * se_boot       # método 1: normal
quantile(dif_boot, c(0.025, 0.975))               # método 2: percentil
