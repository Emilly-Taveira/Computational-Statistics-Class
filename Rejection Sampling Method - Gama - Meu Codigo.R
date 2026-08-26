# Functions
generalized_gama = function(x, rho, alpha, delta) {
  part1 = (rho / alpha^delta) / gamma(delta / rho)
  # Correção do parênteses aplicada aqui:
  part2 = x^(delta - 1) * exp(-(x / alpha)^rho)
  result = part1 * part2
  
  return(result)
}

# Encontrando o valor ideal (máximo) da constante C utilizando método numérico
derivative_equation <- function(x, rho, alpha, delta, a, b) {
  termo1 <- delta - a
  termo2 <- b * x
  termo3 <- (rho / (alpha^rho)) * (x^rho)
  
  return(termo1 + termo2 - termo3)
}

root_results <- uniroot(f = derivative_equation, interval = c(0.001, 5), rho = 2, alpha = 2, delta = 3, a = 2, b = 0.5)

x_root = root_results$root

# Generic Graphs
x = seq(0, 20, length.out = 1000)

gama = dgamma(x, shape = 2, scale = 2)
g_gama = generalized_gama(x, rho = 2, alpha = 2, delta = 3)
ratio = g_gama / gama

par(mfrow = c(1, 2))
plot(x, gama, col = "red", lwd = 2, type = "l", main = "Gráfico da f(x) e g(x)", ylim = c(0, 0.55)) # Função envelope
lines(x, g_gama, lwd = 2) # Distribuição alvo
legend(x = "topright", col = c("red", "black"), lwd = c(2, 2), legend = c("Função Envelope (Gama)", "Distribuição Alvo (Gama generalizada)"), cex = 0.5)
plot(x, ratio, type = "l", main = "Gráfico da Razão f(x)/g(x)", lwd = 2, col = "blue")
abline(v = x_root, , lty = 2, col = "red")
par(mfrow = c(1, 1))

# Plotando a f. envelope com o C ideal
f_max = generalized_gama(x_root, rho = 2, alpha = 2, delta = 3)
g_max = dgamma(x_root, shape = 2, scale = 2)
c_optimum = f_max / g_max

plot(x, g_gama, type = "l", col = "red", lwd = 2, main = "Função Envelope Ajustada e(x) = c.g(x)")
lines(x, c_optimum * gama, , lwd = 2)
legend(x = "topright", col = c("red", "black"), lwd = c(2, 2),
       legend = c("Distribuição Alvo (Gama generalizada)", "Função Envelope (Gama)"))

set.seed(1212)
n = 10000
i = 1
x = numeric(n)

while(i < n) {
  y = rgamma(1, shape = 2, scale = 2)
  u = runif(1)
  
  if(u <= generalized_gama(y, rho = 2, alpha = 2, delta = 3)/ (c_optimum * dgamma(y, shape = 2, scale = 2))){
    x[i] = y
    i = i + 1
  }
}

hist(x, prob = T)
x_seq = seq(min(x), max(x), length.out = 1000)
lines(x_seq, generalized_gama(x_seq, rho = 2, alpha = 2, delta = 3), col = "darkred", lwd = 2)
legend("topright", 
       legend = c("Amostra Simulada", "Gama Generalizada Teórica"), 
       fill = c("gray80", NA), 
       border = c("white", NA),
       col = c(NA, "darkred"), 
       lwd = c(NA, 2), 
       lty = c(NA, 1),
       cex = 0.8)

# 1. Plota a empírica
plot(ecdf(x), main = "FDA Empírica vs Teórica")

curve(pgamma((x/2)^2, shape = 3/2, rate = 1), 
      add = TRUE, 
      col = "red", 
      lwd = 2, lty = 2)
