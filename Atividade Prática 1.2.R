# ===========================================
# Função Quantil da Distribuição Rayleigh
# ===========================================

# Objetivos
# (i) Implementar a função quantil 
# (ii) Calcular os quanrtis para lambda = 0.5, 1.0, 2.0
# (iii) Gráfico da função quantil

# (i) Implementar a função quantil 
qrayleigh_custom = function(p, sigma) {
  quant = sigma * sqrt(-2 * log(1 - p))
}

# (ii) Calcular os quanrtis para sigma = 0.5, 1.0, 2.0

seq_p = seq(0, 1, by = 0.05)
seq_sigma = c(0.5, 1, 2)

q1 = qrayleigh_custom(p = seq_p[6], sigma = seq_sigma)
q2 = qrayleigh_custom(p = seq_p[11], sigma = seq_sigma)
q3 = qrayleigh_custom(p = seq_p[16], sigma = seq_sigma)

tab = data.frame(Q1 = q1, Q2 = q2, Q3 = q3)
print(tab)

# (iii) Gráfico da função quantil

seq_p = seq(0, 1, 0.01)
quant05 = qrayleigh_custom(seq_p, 0.5)
quant10 = qrayleigh_custom(seq_p, 1)
quant20 = qrayleigh_custom(seq_p, 2)

plot(seq_p, quant05, lwd = 2, col = "red", type = "l", ylim = c(0, 4))
lines(seq_p, quant10, lwd = 2, col = "darkgreen", type = "l")
lines(seq_p, quant20, lwd = 2, col = "darkblue", type = "l")
