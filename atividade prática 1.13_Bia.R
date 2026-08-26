##======================================
#Atividade prática 1.13 (burr discreta)
##======================================


prob_burr <- function(x, theta, alpha) {
  
  term1 <- theta^(log(1 + x^alpha))
  term2 <- theta^(log(1 + (1 + x)^alpha))
  return(term1 - term2)
}


fda_burr <- function(x, theta, alpha) {
  
  return(1 - theta^(log(1 + (1 + x)^alpha)))
}


rburr_discreta <- function(n, theta, alpha) {
  u <- runif(n)
  x_simulado <- numeric(n)
  
  for (i in 1:n) {
    x <- 0
    
    while (fda_burr(x, theta, alpha) < u[i]) {
      x <- x + 1
    }
    x_simulado[i] <- x
  }
  return(x_simulado)
}


n_amostra <- 5000
cenarios <- list(
  list(theta = 0.3, alpha = 1.5, nome = "Cenário 1"),
  list(theta = 0.7, alpha = 2.0, nome = "Cenário 2")
)


par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))


for (cen in cenarios) {
  theta <- cen$theta
  alpha <- cen$alpha
  
  
  set.seed(123) 
  amostra <- rburr_discreta(n_amostra, theta, alpha)
  
  tab_freq <- table(amostra) / n_amostra
  x_valphas <- as.numeric(names(tab_freq))
  teorica <- prob_burr(x_valphas, theta, alpha)
  
  bplot <- barplot(tab_freq, 
                   main = paste(cen$nome, ": theta =", theta, ", alpha =", alpha),
                   xlab = "x", ylab = "Probabilidade ",
                   col = "blue", border = "white",
                   ylim = c(0, max(c(tab_freq, teorica)) * 1.2))
  
  points(x = bplot, y = teorica, col = "pink", pch = 16, cex = 1.2)
  lines(x = bplot, y = teorica, col = "pink", lwd = 2)
  
  legend("topright", legend = c("Amostra", "Burr"),
         fill = c("blue", NA), border = c("black", NA),
         pch = c(NA, 16), lty = c(NA, 1), col = c(NA, "pink"), bty = "n")
  

  estatisticas <- c(
    Media = mean(amostra),
    Mediana = median(amostra),
    DP = sd(amostra),
    Min = min(amostra),
    Q1 = quantile(amostra, 0.25),
    Q2 = quantile(amostra, 0.50),
    Q3 = quantile(amostra, 0.75),
    Max = max(amostra)
  )
  
  
  quantis_teoricos <- sapply(c(0.25, 0.50, 0.75), function(u_valpha) {
    x_t <- 0
    while(fda_burr(x_t, theta, alpha) < u_valpha) x_t <- x_t + 1
    return(x_t)
  })
  
  cat("\n---", cen$nome, "---")
  print(round(estatisticas, 4))
  cat("Quantis Teóricos (u=0.25, 0.50, 0.75):", quantis_teoricos, "\n")
}