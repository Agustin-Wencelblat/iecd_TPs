# IECD - Practica 2: Ejercicio 14
# Dada una muestra de una exponencial, comparamos la F empirica contra un estimador parametrico

set.seed(123)
lambda0 <- 1

# Abstraigo los incisos (a),(b) y (c) sobre una funcion para repetirlos con distintos n en (d)
ejercicio14 <- function(n, lambda0 = 1) {
  # (a) Generar muestra
  x <- rexp(n, rate = lambda0)
  
  # (b) Estimadores momentos y emv
  lambda_mom <- 1 / mean(x)
  lambda_emv <- n / sum(x)
  
  plot(ecdf(x), do.points = FALSE, verticals = TRUE, col = "grey40",
       main = paste0("n = ", n), xlab = "x", ylab = "F(x)")
  curve(pexp(x, rate = lambda0), add = TRUE, lwd = 2, lty = 2) # F verdadera
  curve(pexp(x, rate = lambda_emv), add = TRUE, lwd = 2, col = "red") # c) plug-in
  legend("bottomright", inset = 0.03, bg = "white", box.col = "grey80", cex = 1.1,
         legend = c("F empírica", "F verdadera", "F paramétrica"),
         col = c("grey40", "black", "red"), lty = c(1, 2, 1), lwd = 2)
  
  c(n = n, lambda_mom = lambda_mom, lambda_emv = lambda_emv)
}

par(mfrow = c(1, 3))
rbind(ejercicio14(10), ejercicio14(100), ejercicio14(1000))   # (d) Repito con distintos n
par(mfrow = c(1, 1))

