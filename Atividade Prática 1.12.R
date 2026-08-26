library(ggplot2)

prob_lindley <- function(x, beta) {
  (1 + x) * exp(-beta * (x + 2)) * (exp(beta) - 1)^2
}

fda_lindley <- function(x, beta) {
  sapply(x, function(xi) {
    if (xi < 0) return(0)
    k <- 0:xi
    sum((1 + k) * exp(-beta * (k + 2)) * (exp(beta) - 1)^2)
  })
}

rlindley_discreta <- function(n, beta) {
  u <- runif(n)
  x <- numeric(n)
  
  for (i in 1:n) {
    k <- 0
    while (fda_lindley(k, beta) < u[i]) {
      k <- k + 1
    }
    x[i] <- k
  }
  return(x)
}

# Fixando semente para garantir a mesma amostra se rodar de novo
set.seed(2026)
n_sim <- 5000

amostra_b05 <- rlindley_discreta(n_sim, beta = 0.5)
amostra_b15 <- rlindley_discreta(n_sim, beta = 1.5)

# -------------------------------------------------------------------------
# 6. VISUALIZAÇÕES GRÁFICAS
# -------------------------------------------------------------------------
plot_lindley <- function(amostra, beta_val) {
  df_obs <- as.data.frame(table(x = amostra))
  df_obs$x <- as.numeric(as.character(df_obs$x))
  df_obs$freq_rel <- df_obs$Freq / sum(df_obs$Freq)
  
  x_max <- max(df_obs$x)
  df_teo <- data.frame(x = 0:x_max)
  df_teo$prob <- prob_lindley(df_teo$x, beta_val)
  
  ggplot() +
    geom_bar(data = df_obs, aes(x = x, y = freq_rel, fill = "Frequência Observada"), 
             stat = "identity", alpha = 0.7) +
    geom_line(data = df_teo, aes(x = x, y = prob, color = "Probabilidade Teórica"), 
              linewidth = 1) +
    geom_point(data = df_teo, aes(x = x, y = prob, color = "Probabilidade Teórica"), 
               size = 2) +
    scale_fill_manual(name = "", values = c("Frequência Observada" = "steelblue")) +
    scale_color_manual(name = "", values = c("Probabilidade Teórica" = "red")) +
    labs(title = paste("Simulação Lindley Discreta - Série Infinita (\u03B2 =", beta_val, ")"),
         x = "Valores Observados (x)",
         y = "Proporção / Probabilidade") +
    scale_x_continuous(breaks = 0:x_max) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

grafico_b05 <- plot_lindley(amostra_b05, 0.5)
grafico_b15 <- plot_lindley(amostra_b15, 1.5)

# -------------
# ESTATÍSTICAS 
# -------------
calcular_estatisticas <- function(amostra) {
  c(
    Média = mean(amostra),
    Mediana = median(amostra),
    Desvio_Padrão = sd(amostra),
    Q1 = quantile(amostra, 0.25, names = FALSE),
    Q2 = quantile(amostra, 0.50, names = FALSE),
    Q3 = quantile(amostra, 0.75, names = FALSE),
    Mínimo = min(amostra),
    Máximo = max(amostra)
  )
}

tabela_descritiva <- data.frame(
  Estatística = names(calcular_estatisticas(amostra_b05)),
  Beta_0.5 = calcular_estatisticas(amostra_b05),
  Beta_1.5 = calcular_estatisticas(amostra_b15)
)

print("--- Estatísticas Descritivas ---")
print(tabela_descritiva, row.names = FALSE)

# -------------------------------------------------------------------------
# 8. COMPARAÇÃO DE QUANTIS (Amostral vs Teórico)
# -------------------------------------------------------------------------
quantil_teorico_seq <- function(u_vec, beta) {
  sapply(u_vec, function(u) {
    k <- 0
    while (fda_lindley(k, beta) < u) { k <- k + 1 }
    return(k)
  })
}

u_valores <- c(0.25, 0.50, 0.75)
q_teo_b05 <- quantil_teorico_seq(u_valores, 0.5)
q_teo_b15 <- quantil_teorico_seq(u_valores, 1.5)

tabela_quantis <- data.frame(
  Quantil = c("Q1 (u=0.25)", "Q2 (u=0.50)", "Q3 (u=0.75)"),
  Amostral_Beta_0.5 = quantile(amostra_b05, u_valores, names = FALSE),
  Teórico_Beta_0.5 = q_teo_b05,
  Amostral_Beta_1.5 = quantile(amostra_b15, u_valores, names = FALSE),
  Teórico_Beta_1.5 = q_teo_b15
)

print("--- Comparação de Quantis ---")
print(tabela_quantis, row.names = FALSE)

print(grafico_b05)
print(grafico_b15)

