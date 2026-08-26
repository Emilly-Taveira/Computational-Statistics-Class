# ================================================
# Função Quantil da Distribuição Gompertz Discreta
# ================================================

# Função Quantil Contínua
q_gompertz_continua <- function(p, eta, gamma) {
  quantil = (1/gamma) * log(1 - (1 / eta) * log(1 - p))
  return(quantil)
}

# Função Quantil Discreta
q_gompertz_discreta <- function(p, eta, gamma) {
  quant_cont <- q_gompertz_continua(p, eta, gamma)
  quant_disc <- floor(quant_cont) 
  return(quant_disc)
}

# Tabela
etas <- c(0.5, 1.0, 2.0)
gamas <- c(0.5, 1.0, 0.5) 
u_quartis <- c(0.25, 0.50, 0.75)

resultados <- data.frame()

for (i in 1:length(etas)) {
  e <- etas[i]
  g <- gamas[i]
  
  q_cont <- q_gompertz_continua(u_quartis, e, g)
  q_disc <- q_gompertz_discreta(u_quartis, e, g)
  
  df_temp <- data.frame(
    Tipo = c("Contínuo", "Discreto"),
    Eta = e,
    Gama = g,
    Q1_u0.25 = c(q_cont[1], q_disc[1]),
    Q2_u0.50 = c(q_cont[2], q_disc[2]),
    Q3_u0.75 = c(q_cont[3], q_disc[3])
  )
  resultados <- rbind(resultados, df_temp)
}

print("Tabela de Resultados:")
print(resultados)

par(mfrow = c(1, 3))

# Plotando para eta = 0.5, gamma = 0.5
u_seq <- seq(0.01, 0.99, by = 0.01)

plot(u_seq, q_gompertz_continua(u_seq, eta = 0.5, gamma = 0.5), 
     type = "l", col = "green", lwd = 2,
     ylab = "Quantil", xlab = "u", main = "(eta = 0.5, gamma = 0.5)")

lines(u_seq, q_gompertz_discreta(u_seq, eta = 0.5, gamma = 0.5), 
      type = "s", col = "darkred", lwd = 2)

# Plotando para  eta = 1, gamma = 1
u_seq <- seq(0.01, 0.99, by = 0.01)

plot(u_seq, q_gompertz_continua(u_seq, eta = 1, gamma = 1), 
     type = "l", col = "green", lwd = 2,
     ylab = "Quantil", xlab = "u", main = "(eta = 1, gamma = 1)")

lines(u_seq, q_gompertz_discreta(u_seq, eta = 1, gamma = 1), 
      type = "s", col = "darkred", lwd = 2)


# Plotando para eta = 2, gamma = 0.5
u_seq <- seq(0.01, 0.99, by = 0.01)

plot(u_seq, q_gompertz_continua(u_seq, eta = 2, gamma = 0.5), 
     type = "l", col = "green", lwd = 2,
     ylab = "Quantil", xlab = "u", main = "(eta = 2, gamma = 0.5)")

lines(u_seq, q_gompertz_discreta(u_seq, eta = 2, gamma = 0.5), 
      type = "s", col = "darkred", lwd = 2)

