# ================================================
# Função Quantil da Distribuição Lindley Discreta
# ================================================

library(lamW)

# Função Quantil Contínua
q_lindley_continua <- function(u, theta) {
  aux1 <- -(1 + theta) * (1 - u)
  aux2 <- exp(-(1 + theta))
  quant <- -1 - (1/theta) - (1/theta) * lambertWm1(aux1 * aux2)
  return(quant)
}

# Função Quantil Discreta
q_lindley_discreta <- function(u, theta) {
  quant_cont <- q_lindley_continua(u, theta)
  quant_disc <- floor(quant_cont) 
  return(quant_disc)
}

thetas <- c(0.5, 1.0, 2.0)
u_quartis <- c(0.25, 0.50, 0.75)

# Matriz para guardar os resultados
resultados <- data.frame()

for (th in thetas) {
  q_cont <- q_lindley_continua(u_quartis, th)
  q_disc <- q_lindley_discreta(u_quartis, th)
  
  df_temp <- data.frame(
    Tipo = c("Contínuo", "Discreto"),
    Theta = th,
    Q1_u0.25 = c(q_cont[1], q_disc[1]),
    Q2_u0.50 = c(q_cont[2], q_disc[2]),
    Q3_u0.75 = c(q_cont[3], q_disc[3])
  )
  resultados <- rbind(resultados, df_temp)
}
print(resultados)

par(mfrow = c(1, 3))

# Plotando para theta = 0.5
u_seq <- seq(0.01, 0.99, by = 0.01)

plot(u_seq, q_lindley_continua(u_seq, theta = 0.5), 
     type = "l", col = "darkgreen", lwd = 2,
     ylab = "Quantil", xlab = "u", main = "(Theta = 0.5)")

lines(u_seq, q_lindley_discreta(u_seq, theta = 0.5), 
      type = "s", col = "red", lwd = 2)

# Plotando para theta = 1.0 
u_seq <- seq(0.01, 0.99, by = 0.01)

plot(u_seq, q_lindley_continua(u_seq, theta = 1.0), 
     type = "l", col = "darkgreen", lwd = 2,
     ylab = "Quantil", xlab = "u", main = "(Theta = 1.0)")

lines(u_seq, q_lindley_discreta(u_seq, theta = 1.0), 
      type = "s", col = "red", lwd = 2)


# Plotando para theta = 2
u_seq <- seq(0.01, 0.99, by = 0.01)

plot(u_seq, q_lindley_continua(u_seq, theta = 2), 
     type = "l", col = "darkgreen", lwd = 2,
     ylab = "Quantil", xlab = "u", main = "(Theta = 2.0)")

lines(u_seq, q_lindley_discreta(u_seq, theta = 2), 
      type = "s", col = "red", lwd = 2)
