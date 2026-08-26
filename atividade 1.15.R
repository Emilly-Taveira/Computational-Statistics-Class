# ==============================================================================
# Atividade Prática 1.15 - Método da Rejeição (Distribuição Gama)
# ==============================================================================


set.seed(123)

# Parâmetros
n <- 1000
alpha <- 3
beta <- 2
lambda <- 0.5


dgama_target <- function(x) {
  dgamma(x, shape = alpha, rate = beta)
}


dgama_proposal <- function(x) {
  dexp(x, rate = lambda)
}


c_opt <- (beta^alpha / (lambda * gamma(alpha))) * ((alpha - 1) / (beta - lambda))^(alpha - 1) * exp(-(alpha - 1))

cat("Constante ótima de rejeição (c):", c_opt, "\n\n")


rgama_rejection <- function(n, alpha, beta, lambda, c) {
  amostra <- numeric(n)
  contador <- 1
  
  while (contador <= n) {
   
    y <- rexp(1, rate = lambda)
    
    
    u <- runif(1)
    
    
    f_y <- dgamma(y, shape = alpha, rate = beta)
    g_y <- dexp(y, rate = lambda)
    
    if (u <= f_y / (c * g_y)) {
      amostra[contador] <- y
      contador <- contador + 1
    }
  }
  return(amostra)
}


amostra_gerada <- rgama_rejection(n, alpha, beta, lambda, c_opt)


teste_ks <- ks.test(amostra_gerada, "pgamma", shape = alpha, rate = beta)

estatistica_Dn <- teste_ks$statistic
p_valor <- teste_ks$p.value

cat("--- Resultados do Teste KS (1 Amostra) ---\n")
cat("Estatística Dn observada:", estatistica_Dn, "\n")
cat("P-valor do teste:", p_valor, "\n")

if(p_valor > 0.05) {
  cat("Conclusão: Não rejeitamos a hipótese nula a 5% de significância. A amostra adere à distribuição teórica.\n\n")
} else {
  cat("Conclusão: Rejeitamos a hipótese nula a 5% de significância. A amostra difere da distribuição teórica.\n\n")
}


par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))


hist(amostra_gerada, probability = TRUE, main = "Histograma vs Densidade",
     xlab = "Valores de x", ylab = "Densidade", col = "pink", border = "white",
     ylim = c(0, max(dgama_target(amostra_gerada)) * 1.2))
curve(dgama_target(x), add = TRUE, col = "purple", lwd = 2)


plot(ecdf(amostra_gerada), main = "F.D.A. Empírica vs Teórica",
     xlab = "Valores de x", ylab = "Probabilidade Acumulada", col = "blue", lwd = 1.5)
curve(pgamma(x, shape = alpha, rate = beta), add = TRUE, col = "purple", lwd = 2, lty = 2)


par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

B <- 100
p_valores_rep <- numeric(B)

for (i in 1:B) {
  amostra_rep <- rgama_rejection(n, alpha, beta, lambda, c_opt)
  ks_rep <- ks.test(amostra_rep, "pgamma", shape = alpha, rate = beta)
  p_valores_rep[i] <- ks_rep$p.value
}

taxa_aceitacao_teste <- sum(p_valores_rep > 0.05) / B

cat("--- Resultados para B = 100 Replicações ---\n")
cat("Taxa de aceitação da hipótese nula (esperado ~0.95):", taxa_aceitacao_teste, "\n")