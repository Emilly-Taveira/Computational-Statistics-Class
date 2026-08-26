# ===========================================
# Função Quantil da Distribuição Poisson
# ===========================================

# SequÊncia de Valores 'u'
u_seq = seq(0.1, 0.9, by = 0.1)

# Valores do parâmetro 'lambda'
lambda = 2

# Função quantil (Definição 1.3)

qpois_custom <- function(p, lambda, limite_k = 100) {
  k = 0:limite_k
  
  fda = ppois(k, lambda)
  quantil = k[min(which(fda >= p))]
  return(quantil)
}

# Cálculos dos quantis

Q_Poisson_custom = sapply(u_seq, 
                          function(x) qpois_custom(x, lambda))
Q_Poisson_R = qpois(u_seq, lambda)

result = cbind(Q_Poisson_R, Q_Poisson_custom)
rownames(result) = u_seq
result

