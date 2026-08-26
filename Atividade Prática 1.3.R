# ===========================================
# Função Quantil da Distribuição Lindley
# ===========================================

# Objetivos
# (i) Implementar a função quantil 
# (ii) Calcular os quanrtis para lambda = 0.5, 1.0, 2.0
# (iii) Gráfico da função quantil

install.packages("lamW")
library(lamW)

# (i) Implementar a função quantil 
qlindley_custom = function(p, theta) {
  quant = -(1/theta) - (1/theta) * lambertWm1(-(1 + theta) * (1 - p) * exp(-(1 + theta)))
}

qlindley_custom = function(p, theta) {
  aux1 = -(1 + theta) * (1 - p)
  aux2 = exp(-(1 + theta))
  quant = -(1/theta) - (1/theta) * lambertWm1(aux1 * aux2)
}

# (ii) Calcular os quanrtis para theta = 0.5, 1.0, 2.0

seq_p = seq(0, 1, by = 0.05)
seq_theta = c(0.5, 1, 2)

q1 = qlindley_custom(p = seq_p[6], theta = seq_theta)
q2 = qlindley_custom(p = seq_p[11], theta = seq_theta)
q3 = qlindley_custom(p = seq_p[16], theta = seq_theta)

tab = data.frame(Q1 = q1, Q2 = q2, Q3 = q3)
print(tab)

# (iii) Gráfico da função quantil

seq_p = seq(0, 1, 0.01)
quant05 = qlindley_custom(seq_p, 0.5)
quant10 = qlindley_custom(seq_p, 1)
quant20 = qlindley_custom(seq_p, 2)

plot(seq_p, quant05, lwd = 2, col = "red", type = "l")
lines(seq_p, quant10, lwd = 2, col = "darkgreen", type = "l")
lines(seq_p, quant20, lwd = 2, col = "darkblue", type = "l")

