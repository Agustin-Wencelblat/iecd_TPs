# =============================================================================
# IECD - Práctica 5 (Tests) - Ejercicio 18
# Distribución del p-valor bajo H0
# =============================================================================

set.seed(18)
n    <- 25
Nrep <- 1000

# a) n = 25 datos N(10, 3)  (varianza 3, notación del curso: N(mu, sigma^2))
x <- rnorm(n, mean = 10, sd = sqrt(3))

# b) t.test de H0: mu = 10 vs H1: mu != 10
t.test(x, mu = 10)$p.value

# c) Nrep = 1000 repeticiones
pvalores <- replicate(Nrep, t.test(rnorm(n, mean = 10, sd = sqrt(3)), mu = 10)$p.value)

# d) histograma
hist(pvalores, breaks = seq(0, 1, by = 0.05), freq = FALSE,
     col = "lightblue", border = "white",
     main = "p-valores bajo H0", xlab = "p-valor")
abline(h = 1, col = "red", lwd = 2)          # densidad de la U(0, 1)

# e) probabilidad estimada de rechazar
c(alpha_0.05 = mean(pvalores < 0.05), alpha_0.10 = mean(pvalores < 0.10))

# Chequeo: comparar la F empírica de los p-valores con la de una U(0,1)
plot(ecdf(pvalores), do.points = FALSE, main = "F empírica de los p-valores")
abline(0, 1, col = "red", lwd = 2)
