# ==============================================================================
# ATIVIDADE 1.14: MÉTODO DA REJEIÇÃO
# =============================================================================

library(ggplot2)
library(patchwork)

set.seed(123)
n_sim <- 10000


rodar_simulacao <- function(n, f, g, r_prop, c) {
  amostras <- numeric(n)
  aceitos <- 0
  tentativas <- 0
  
  tempo_inicial <- Sys.time()
  while(aceitos < n) {
    tentativas <- tentativas + 1
    y <- r_prop(1)
    u <- runif(1)
    if(u <= f(y) / (c * g(y))) {
      aceitos <- aceitos + 1
      amostras[aceitos] <- y
    }
  }
  tempo_final <- Sys.time()
  
  return(list(
    amostras = amostras, 
    taxa_obs = n / tentativas,
    tempo = as.numeric(difftime(tempo_final, tempo_inicial, units = "secs"))
  ))
}



# CENÁRIO 1
f1 <- function(x) dnorm(x)
g1 <- function(x) 0.5 * exp(-abs(x))
r_prop1 <- function(n) { 
  u <- runif(n, -0.5, 0.5)
  ifelse(u < 0, log(1 + 2*u), -log(1 - 2*u)) 
}
c1 <- sqrt(2 * exp(1) / pi)
res1 <- rodar_simulacao(n_sim, f1, g1, r_prop1, c1)

# CENÁRIO 2
f2 <- function(x) dgamma(x, 4, 1)
g2 <- function(x) dgamma(x, 4, 0.8)
r_prop2 <- function(n) rgamma(n, 4, 0.8)
c2 <- optimize(f = function(x) f2(x)/g2(x), interval = c(0, 50), maximum = TRUE)$objective
res2 <- rodar_simulacao(n_sim, f2, g2, r_prop2, c2)

# CENÁRIO 3
f3 <- function(x) dbeta(x, 2.5, 2.5)
g3 <- function(x) dunif(x, 0, 1)
r_prop3 <- function(n) runif(n, 0, 1)
c3 <- f3(0.5)
res3 <- rodar_simulacao(n_sim, f3, g3, r_prop3, c3)


tabela_final <- data.frame(
  Cenario = c("C1: Normal", "C2: Gama", "C3: Beta"),
  Constante_c = c(c1, c2, c3),
  Taxa_Esperada_1c = 1 / c(c1, c2, c3),
  Taxa_Observada = c(res1$taxa_obs, res2$taxa_obs, res3$taxa_obs),
  Tempo_Execucao_Seg = c(res1$tempo, res2$tempo, res3$tempo)
)

print("--- TABELA DE RESULTADOS DA ATIVIDADE ---")
print(tabela_final)


gerar_coluna <- function(res, f, g, c, x_lim) {
  x_seq <- seq(x_lim[1], x_lim[2], length.out = 1000)
  df_c <- data.frame(x = x_seq, alvo = f(x_seq), env = c * g(x_seq))
  
  p1 <- ggplot(df_c, aes(x)) +
    geom_line(aes(y = alvo), color = "pink") +
    geom_line(aes(y = env), color = "purple", linetype = "dashed") +
    theme_minimal() + labs(x = NULL, y = NULL)
  
  p2 <- ggplot(data.frame(v = res$amostras), aes(v)) +
    geom_histogram(aes(y = ..density..), bins = 40, fill = "pink", color = "white") +
    stat_function(fun = f, color = "black") +
    theme_minimal() + labs(x = NULL, y = NULL)
  
  return(p1 / p2)
}

col1 <- gerar_coluna(res1, f1, g1, c1, c(-4, 4))
col2 <- gerar_coluna(res2, f2, g2, c2, c(0, 15))
col3 <- gerar_coluna(res3, f3, g3, c3, c(0, 1))

layout_final <- (col1 | col2 | col3) + 
  plot_annotation(
    title = "Resultados: Método da Rejeição",
    subtitle = "Cenário 1: Normal | Cenário 2: Gama | Cenário 3: Beta",
    caption = "Linha superior: Envelopes (c*g) | Linha inferior: Histogramas das Amostras"
  )

print(layout_final)