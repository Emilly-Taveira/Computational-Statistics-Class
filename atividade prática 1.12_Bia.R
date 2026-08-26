##==================================
#Atividade prática 1.12(lindley discreta)
##=================================

# Função de Proba
prob_Lindley <- function(x, beta) {
  term <- (exp(beta) - 1)^2
  prob <- (1 + x) * exp(-beta * (x + 2)) * term
  return(prob)
}

# Função de Distribuição Acumulada 
fda_Lindley <- function(x, beta) {
  
  sapply(x, function(xi) {
    k <- 0:xi
    sum(prob_Lindley(k, beta))
  })
}

rlindley_discreta <- function(n, beta) {
  amostra <- numeric(n)
  for (i in 1:n) {
    u <- runif(1)
    x <- 0
    fda <- prob_Lindley(0, beta) # F(0)
    while (fda < u) {
      x <- x + 1
      fda <- fda + prob_Lindley(x, beta)
    }
    amostra[i] <- x
  }
  return(amostra)
}

set.seed(123) 
betas <- c(0.5, 1.5)
n <- 5000

par(mfrow = c(1, 2))

for (b in betas) {
  # Gerar amostra
  dados <- rlindley_discreta(n, b)
  tab_relativa <- table(dados) / n
  x_vals <- as.numeric(names(tab_relativa))
  
  barplot(tab_relativa, 
          main = paste(" Lindley Discreta", b, ""),
          xlab = "x", ylab = "Probabilidade",
          col = "pink", border = "white")
  
  probs_teoricas <- prob_Lindley(x_vals, b)
  points(x = seq_along(x_vals) * 1.2 - 0.5, 
         y = probs_teoricas, col = "black", pch = 16, cex = 1.2)
  lines(x = seq_along(x_vals) * 1.2 - 0.5, 
        y = probs_teoricas, col = "black", lty = 2)
  
  cat("\n--- Estatísticas para beta =", b, "---\n")
  stats <- c(
    Média = mean(dados),
    Mediana = median(dados),
    Desvio_Padrao = sd(dados),
    Q1 = quantile(dados, 0.25),
    Q2 = quantile(dados, 0.50),
    Q3 = quantile(dados, 0.75),
    Min = min(dados),
    Max = max(dados)
  )
  print(round(stats, 4))
  
  u_alvos <- c(0.25, 0.50, 0.75)
  quantis_teoricos <- sapply(u_alvos, function(u) {
    x_t <- 0
    while(fda_Lindley(x_t, b) < u) { x_t <- x_t + 1 }
    return(x_t)
  })
  names(quantis_teoricos) <- paste0("Q_teorico_", u_alvos)
  cat("Quantis Teóricos:\n")
  print(quantis_teoricos)
}