# ===========================================
# Função Quantil da Distribuição Exponencial
# ===========================================

# Objetivos
# (i) Implementar a função quantil 
# (ii) Calcular os quanrtis para lambda = 0.5, 1.0, 2.0
# (iii) Gráfico da função quantil

rm(list = setdiff(ls(), "df"))

# (i) Implementar a função quantil 
qexp_custom = function(p, lambda) {
  quant = -1/lambda * log(1 - p)
}

# (ii) Calcular os quanrtis para lambda = 0.5, 1.0, 2.0

seq_p = seq(0, 1, by = 0.05)
seq_lambda = c(0.5, 1, 2)

q1 = qexp_custom(p = seq_p[6], lambda = seq_lambda)
q2 = qexp_custom(p = seq_p[11], lambda = seq_lambda)
q3 = qexp_custom(p = seq_p[16], lambda = seq_lambda)

tab = data.frame(Q1 = q1, Q2 = q2, Q3 = q3)
print(tab)

# (iii) Gráfico da função quantil

seq_p = seq(0, 1, 0.01)
quant05 = qexp_custom(seq_p, 0.5)
quant10 = qexp_custom(seq_p, 1)
quant20 = qexp_custom(seq_p, 2)

plot(seq_p, quant05, lwd = 2, col = "red", type = "l")
lines(seq_p, quant10, lwd = 2, col = "darkgreen", type = "l")
lines(seq_p, quant20, lwd = 2, col = "darkblue", type = "l")
