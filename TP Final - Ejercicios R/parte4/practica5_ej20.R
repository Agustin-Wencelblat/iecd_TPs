# =============================================================================
# IECD - Práctica 5 (Tests) - Ejercicio 20
# Nacimientos por día de la semana (EE.UU., 1977, 5000 nacimientos)
# =============================================================================

W <- c(lunes = 747, martes = 766, miercoles = 749, jueves = 741,
       viernes = 714, sabado = 659, domingo = 624)
n     <- sum(W)            # 5000
th0   <- 1 / 7
alpha <- 0.05
z     <- qnorm(1 - alpha / 2)

# a) y b) Un test por día: H0: theta_j = 1/7 vs H1: theta_j != 1/7
theta_hat <- W / n
Z      <- sqrt(n) * (theta_hat - th0) / sqrt(th0 * (1 - th0))
pvalor <- 2 * (1 - pnorm(abs(Z)))
round(cbind(theta_hat, Z, pvalor, rechaza = abs(Z) > z), 4)

# c) Test psi: rechaza si alguno de los 7 rechaza.
#    Cota de Bonferroni: nivel <= 7 * 0.05 = 0.35. Lo estimamos por simulación bajo H0.
set.seed(20)
Nrep <- 10000
sim  <- rmultinom(Nrep, size = n, prob = rep(th0, 7))     # cada columna: un W bajo H0
Zsim <- sqrt(n) * (sim / n - th0) / sqrt(th0 * (1 - th0))
c(nivel_psi       = mean(apply(abs(Zsim) > z, 2, any)),
  nivel_psi_bonf  = mean(apply(abs(Zsim) > qnorm(1 - alpha / (2 * 7)), 2, any)))

# d) Test de cociente de máxima verosimilitud (multinomial)
#    -2 log lambda = 2 sum W_j log(W_j / (n/7))  ~  chi^2_6 bajo H0
E  <- n * th0
CV <- 2 * sum(W * log(W / E))
c(estadistico = CV, critico = qchisq(1 - alpha, df = 6),
  pvalor = pchisq(CV, df = 6, lower.tail = FALSE))
# Comparación con el estadístico de Pearson (chisq.test)
chisq.test(W, p = rep(th0, 7))

# e) Distribución del estadístico de CMV bajo H0, simulada
estadsimu <- apply(sim, 2, function(w) 2 * sum(ifelse(w > 0, w * log(w / E), 0)))
c(cuantil_simulado = unname(quantile(estadsimu, 0.95)),
  cuantil_chi2_6   = qchisq(0.95, df = 6))

plot(density(estadsimu), lwd = 2, main = "Estadístico CMV bajo H0",
     xlab = expression(-2 * log(lambda)))
curve(dchisq(x, df = 6), add = TRUE, col = "red", lwd = 2, lty = 2)
abline(v = CV, col = "darkgreen", lwd = 2)
legend("topright", inset = c(0.15, 0), bty = "n", lwd = 2, lty = c(1, 2, 1),
       col = c("black", "red", "darkgreen"),
       legend = c("densidad simulada", expression(chi[6]^2), "valor observado"))
