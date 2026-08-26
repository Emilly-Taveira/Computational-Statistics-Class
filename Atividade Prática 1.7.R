# ===========================================
# Função Quantil da Distribuição Geométrica
# ===========================================

# SequÊncia de Valores 'u'
u_seq = seq(0.1, 0.9, by = 0.1)

# Valores do parâmetro 'lambda'
theta = 0.5

# Função quantil (Definição 1.3)

qgeom_def <- function(p, theta, limite_k = 100) {
  k = 0:limite_k
  
  fda = pgeom(q = k, prob = theta)
  quantil = k[min(which(fda >= p))]
  return(quantil)
}

# Função quantil (Analítica)

qgeom_analitica <-  function(p, theta){
  aux1 = log(1 - p)
  aux2 = log(1 - theta)
  quantil = ceiling(aux1/aux2 - 1)
  return(quantil)
}


# Cálculos dos quantis

Q_geom_D = sapply(u_seq, 
                          function(x) qgeom_def(x, theta))
Q_geom_A = sapply(u_seq, 
                  function(x) qgeom_analitica(x, theta))
Q_geom_R = qgeom(u_seq, theta)

result = cbind(Q_geom_D, Q_geom_A, Q_geom_R)
rownames(result) = u_seq
result

