# ===========================================
# Função Quantil da Distribuição Pareto
# ===========================================

# Objetivos
# (i) Implementar a função quantil 
# (ii) Calcular os quanrtis para lambda = 0.5, 1.0, 2.0
# (iii) Gráfico da função quantil

rm(list = setdiff(ls(), "df"))

# (i) Implementar a função quantil 
qpareto_custom = function(p, alpha, beta) {
  quant = beta / (1 - p)^(1 / alpha)
}

# (ii) Calcular os quantis para alpha = 0.5, 1.0, 2.0

seq_p = seq(0, 1, by = 0.05)
seq_alpha = c(1, 2, 3)
seq_beta = c(1, 1, 2)

q1 = qpareto_custom(p = seq_p[6], alpha = seq_alpha, beta = seq_beta)
q2 = qpareto_custom(p = seq_p[11], alpha = seq_alpha, beta = seq_beta)
q3 = qpareto_custom(p = seq_p[16], alpha = seq_alpha, beta = seq_beta)

tab = data.frame(Q1 = q1, Q2 = q2, Q3 = q3)
print(tab)

# (iii) Gráfico da função quantil

seq_p = seq(0, 1, 0.01)
quant_parametros1 = qpareto_custom(seq_p, alpha = 1, beta = 1)
quant_parametros2 = qpareto_custom(seq_p, alpha = 2, beta = 1)
quant_parametros3 = qpareto_custom(seq_p, alpha = 3, beta = 2)

plot(seq_p, quant_parametros1, lwd = 2, col = "red", type = "l", ylim = c(-10, 80))
lines(seq_p, quant_parametros2, lwd = 2, col = "darkgreen", type = "l")
lines(seq_p, quant_parametros3, lwd = 2, col = "darkblue", type = "l")

