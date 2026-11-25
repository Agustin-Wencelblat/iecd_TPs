# =============================================================================
# IECD - Práctica 5 (Tests) - Ejercicio 23 (Debernardi.csv)
# Comparación de log(LYVE1) entre los tres grupos de diagnosis
# =============================================================================

datos <- read.csv("Debernardi.csv")
datos$diagnosis <- factor(datos$diagnosis)
datos$logLYVE1 <- log(datos$LYVE1)
colores <- c("1" = "#1b9e77", "2" = "#d95f02", "3" = "#7570b3")

# -----------------------------------------------------------------------------
# a) QQ-plots normales y boxplots, para LYVE1 y log(LYVE1)
# -----------------------------------------------------------------------------
for (v in c("LYVE1", "logLYVE1")) {
  par(mfrow = c(1, 3))
  for (g in levels(datos$diagnosis)) {
    x <- datos[[v]][datos$diagnosis == g]
    qqnorm(x, pch = 19, col = colores[g], main = paste(v, "- diagnosis", g))
    qqline(x, lwd = 2)
  }
  par(mfrow = c(1, 1))
}

par(mfrow = c(1, 2))
boxplot(LYVE1 ~ diagnosis, data = datos, col = colores, main = "LYVE1")
boxplot(logLYVE1 ~ diagnosis, data = datos, col = colores, main = "log(LYVE1)")
par(mfrow = c(1, 1))

# Resumen por grupo
aggregate(logLYVE1 ~ diagnosis, data = datos,
          FUN = function(x) c(n = length(x), media = mean(x), desvio = sd(x)))

# -----------------------------------------------------------------------------
# b) H0: mu1 = mu2 vs H1: mu1 != mu2 para log(LYVE1), nivel 0.01
# -----------------------------------------------------------------------------
x1 <- datos$logLYVE1[datos$diagnosis == 1]
x2 <- datos$logLYVE1[datos$diagnosis == 2]
x3 <- datos$logLYVE1[datos$diagnosis == 3]

var.test(x1, x2)                                   # ¿varianzas iguales?
t.test(x1, x2, var.equal = TRUE,  conf.level = 0.99)  # S_p^2
t.test(x1, x2, var.equal = FALSE, conf.level = 0.99)  # Welch

# -----------------------------------------------------------------------------
# c) Las tres comparaciones con nivel global 0.01 (Bonferroni: 0.01/3 cada una)
# -----------------------------------------------------------------------------
alpha_global <- 0.01
alpha_indiv  <- alpha_global / 3

pares <- list(c("1", "2"), c("1", "3"), c("2", "3"))
res <- t(sapply(pares, function(p) {
  a <- datos$logLYVE1[datos$diagnosis == p[1]]
  b <- datos$logLYVE1[datos$diagnosis == p[2]]
  tt <- t.test(a, b, var.equal = FALSE)            # Welch: varianzas muy distintas
  c(dif_medias = unname(mean(a) - mean(b)), t = unname(tt$statistic),
    pvalor = tt$p.value, rechaza = tt$p.value < alpha_indiv)
}))
rownames(res) <- c("1 vs 2", "1 vs 3", "2 vs 3")
res
alpha_indiv
