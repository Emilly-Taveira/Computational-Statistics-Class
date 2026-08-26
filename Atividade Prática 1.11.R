# =================================================================
# Exemplo: Distribuição Gompertz (Valores Pseudoaleatórios)
# =================================================================

install.packages("DescTools")
library(DescTools)

## Valores Pseudoaleatórios da Distribuição Exponencial

rgompertz_analitico <- function(n, eta, gamma) {
  # 1° passo: gerar U ~ Uniforme(0, 1)
  
  u = runif(n, min = 0, max = 1)
  
  # 2° passo: Cálculo do Quantil
  quantil = (1/gamma) * log(1 - (1 / eta) * log(1 - u))
}

## Teste

x_values = rgompertz_analitico(n = 5000, eta = 1.5, gamma = 0.8)
head(x_values, n = 100)
hist(x_values, col = 'skyblue', probability = T)
curve(dGompertz(x, shape = 1.5, rate = 0.8), col = 'red', lwd = 2, add = T)
