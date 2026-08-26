# =================================================================
# Exemplo: Distribuição Exponencial (Valores Pseudoaleatórios)
# =================================================================

## Valores Pseudoaleatórios da Distribuição Exponencial

rexp_c <- function(n, lambda) {
  # 1° passo: gerar U ~ Uniforme(0, 1)
  
  u = runif(n, min = 0, max = 1)
  
  # 2° passo: Cálculo do Quantil
  x = - (1/lambda) * log(1 - u)
  
}

## Teste

x_values = rexp_c(n = 10000, lambda = 0.5)
head(x_values, n = 100)
hist(x_values, col = 'skyblue', probability = T)
curve(dexp(x, rate = 0.5), col = 'red', lwd = 2, add = T)

