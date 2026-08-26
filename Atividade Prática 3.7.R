## --- Pacotes Necessários --- ##
library(ggplot2)

## --- Parâmetros --- ##
set.seed(2026)
n <- 10000        # Número de simulações
lambda <- 2       # Parâmetro da Poisson (original)
mu <- 2           # Média da Lognormal (original)
sigma <- 0.5      # Desvio padrão da Lognormal
lambda_star <- 8  # Parâmetro da Poisson (importância)
mu_star <- 2.5    # Média da Lognormal (importância)

## --- Função para Simular X = Soma(Y_ij) --- ##
simular_X <- function(n_sim, lam, m, s) {
  X <- replicate(n_sim, {
    N <- rpois(1, lam)
    if(N == 0) return(0)
    Y <- rlnorm(N, meanlog = m, sdlog = s)
    sum(Y)
  })
  return(X)
}

## --- 1. Gerar Amostras e Pesos de Importância --- ##
# Gerando amostras com a distribuição de importância
N_star <- rpois(n, lambda_star)
X_star <- sapply(N_star, function(ni) {
  if(ni == 0) return(0)
  sum(rlnorm(ni, meanlog = mu_star, sdlog = sigma))
})

# Calculando pesos (wi) - Equação 3.149
# w = f(original) / f(importância)
log_w <- (N_star * (log(lambda) - log(lambda_star)) + (lambda_star - lambda)) +
  sapply(1:n, function(i) {
    if(N_star[i] == 0) return(0)
    sum(dlnorm(rlnorm(N_star[i], mu_star, sigma), mu, sigma, log = TRUE) - 
          dlnorm(rlnorm(N_star[i], mu_star, sigma), mu_star, sigma, log = TRUE))
  })
w <- exp(log_w)

## --- 2. Estimar VaR(0.95) via Quantil Ponderado --- ##
# Ordenar X pelos pesos
df <- data.frame(X = X_star, w = w)
df <- df[order(df$X), ]
df$cumsum_w <- cumsum(df$w) / sum(df$w)

# VaR estimado (0.95)
var_095 <- df$X[which.max(df$cumsum_w >= 0.95)]

## --- 3. Resultados --- ##
cat("Estimativa do VaR(0.95):", var_095, "\n")

# Visualização
ggplot(df, aes(x = X)) +
  geom_density(aes(weight = w), fill = "blue", alpha = 0.3) +
  geom_vline(xintercept = var_095, color = "red", linetype = "dashed") +
  labs(title = "Distribuição da Perda (Amostragem por Importância)",
       subtitle = paste("VaR(0.95) =", round(var_095, 2)),
       x = "X", y = "Densidade Ponderada")