# Recozimento Simulado: Distribuição Lindley

# Densidade
dLindley <- function(x, theta, log = FALSE) {
  dens = (theta^2 / (1 + theta)) * (1 + x) * exp(-theta * x)
  if(log) return(log(dens))
  return(dens)
}

# Geração de Valores Pseudoaleatórios
rLindley <- function(n, theta) {
  p <-  theta / (1 + theta)
  u <- runif(n)
  x <- numeric(n)
  idx_exp = u <= p
  idx_gam = !idx_exp
  
  x[idx_exp] = rexp(sum(idx_exp), rate = theta)
  x[idx_gam] = rgamma(sum(idx_gam), shape = 2, rate = theta)
  
  return(x)
}

# Log-verossimilhança

loglike_lindley <- function(x, theta) {
  sum(dLindley(x, theta, log = T))
}

# Valores Iniciais

n = 200
theta_ver = 2.0
x = rLindley(n = n, theta = theta_ver)
theta_0 = 5.0
T0 = 100
alpha = 0.95
n_inter = 1000
sd_prop = 1 # Desvio-Padr]ap da Proposta (Erro Normal)

# Algoritmo: Recozimento Simulado

thetas <-  numeric(n_inter + 1)
custoll <-  numeric(n_inter + 1)
thetas[1] = theta_0
custoll[1] = loglike_lindley(x, theta_0)

for(t in 1:n_inter) {
  # Cronograma de resfriamento geométrico
  #T = T0 * alpha^(t - 1)
  # Cronograma de resfriamento logarítmico
  T = T0 / log(t + 1)
    
  # Proposta (Erro Normal)
  theta_1 = exp(log(theta_0) + rnorm(1, 0, sd_prop))
  
  # Diferenças
  delta = loglike_lindley(x, theta = theta_1) - loglike_lindley(x, theta = theta_0)
  
  # Regra de Decisão (Maximização)
  
  if(delta >= 0 || runif(1) < exp(delta/ T)) {
    theta_0 = theta_1
  }
  
  thetas[t + 1] = theta_0
  custoll[t + 1] = loglike_lindley(x, theta_0)
}

idx_max = which.max(custoll)
EMV_rs = thetas[idx_max]
print(EMV_rs)

hist(x, prob = T)
curve(dLindley(x, theta = EMV_rs), col = 'red', lwd = 2, add = TRUE)
