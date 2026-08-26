## --- Configurações Iniciais --- ##
set.seed(2026)
n <- 10000        # Número de simulações
# Exemplo: Estimar E[exp(U)] onde U ~ Uniform(0,1)
# O valor exato é e - 1 ≈ 1.71828

## --- 1. Método de Monte Carlo Bruto --- ##
u <- runif(n)
mc_bruto <- exp(u)
est_bruto <- mean(mc_bruto)
var_bruto <- var(mc_bruto) / n

## --- 2. Método de Variáveis Antitéticas --- ##
# Gera U e (1-U) para reduzir a variância
u_anti <- runif(n/2)
y1 <- exp(u_anti)
y2 <- exp(1 - u_anti)
mc_antiteticas <- (y1 + y2) / 2
est_anti <- mean(mc_antiteticas)
var_anti <- var(mc_antiteticas) / (n/2)

## --- 3. Variáveis de Controle --- ##
# Usamos U como variável de controle (covariável)
# E[U] = 0.5
c <- -cov(exp(u), u) / var(u) # Coeficiente ótimo
mc_controle <- exp(u) + c * (u - 0.5)
est_controle <- mean(mc_controle)
var_controle <- var(mc_controle) / n

## --- 4. Resultados e Eficiência --- ##
resultados <- data.frame(
  Metodo = c("MC Bruto", "Antitéticas", "Variáveis de Controle"),
  Estimativa = c(est_bruto, est_anti, est_controle),
  Variancia = c(var_bruto, var_anti, var_controle)
)

print(resultados)
cat("\nEficiência Relativa (Bruto/Controle):", var_bruto / var_controle)