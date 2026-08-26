# -------------------------------------------------------------------------
# 1. Bibliotecas e Funções Base
# -------------------------------------------------------------------------
if (!require("ADGofTest")) install.packages("ADGofTest")
if (!require("goftest")) install.packages("goftest")
if (!require("knitr")) install.packages("knitr")

library(ADGofTest)
library(goftest)
library(knitr)

# PDF da Gama Generalizada
dgengamma <- function(x, delta, rho, alpha) {
  term1 <- (rho / (alpha^delta)) / gamma(delta / rho)
  term2 <- x^(delta - 1) * exp(-(x / alpha)^rho)
  return(term1 * term2)
}

# CDF da Gama Generalizada
pgengamma <- function(x, delta, rho, alpha) {
  return(pgamma((x / alpha)^rho, shape = delta / rho))
}

# -------------------------------------------------------------------------
# 2. Algoritmo de Aceitação-Rejeição
# -------------------------------------------------------------------------
rgengamma_ar <- function(n, delta, rho, alpha) {
  samples <- numeric(n)
  count <- 0
  
  shape_p <- delta / rho
  scale_p <- alpha
  
  # Otimização para encontrar o M (supremo da razão f/g)
  f_over_g <- function(x) dgengamma(x, delta, rho, alpha) / dgamma(x, shape = shape_p, scale = scale_p)
  opt <- optimize(f_over_g, interval = c(0, 100), maximum = TRUE)
  M <- opt$objective * 1.01 
  
  while (count < n) {
    x_cand <- rgamma(1, shape = shape_p, scale = scale_p)
    u <- runif(1)
    
    if (u <= dgengamma(x_cand, delta, rho, alpha) / (M * dgamma(x_cand, shape = shape_p, scale = scale_p))) {
      count <- count + 1
      samples[count] <- x_cand
    }
  }
  return(samples)
}

# -------------------------------------------------------------------------
# 3. Configuração de Cenários
# -------------------------------------------------------------------------
cenarios <- data.frame(
  id = 1:6,
  delta = c(1.0, 2.0, 2.0, 0.5, 2.5, 1.5),
  rho   = c(1.0, 1.0, 2.0, 1.0, 2.5, 1.5),
  alpha = c(1.0, 1.0, 1.0, 2.0, 1.0, 0.5)
)

tamanhos <- c(10, 100, 1000, 10000, 100000)
tempos_matriz <- matrix(0, nrow = 6, ncol = length(tamanhos))
colnames(tempos_matriz) <- paste0("N=", tamanhos)

# -------------------------------------------------------------------------
# 4. Execução, Gráficos e Testes
# -------------------------------------------------------------------------
for (j in 1:length(tamanhos)) {
  n_j <- tamanhos[j]
  
  # Armazenar p-valores para o output formatado
  p_values_summary <- character(6)
  
  cat("\n======================================================\n")
  cat(sprintf("      AMOSTRA DE P-VALORES DOS TESTES (N=%s)      ", format(n_j, big.mark=".")))
  cat("\n======================================================\n")
  
  for (i in 1:nrow(cenarios)) {
    par_i <- cenarios[i, ]
    
    # Simulação e Tempo
    start_time <- Sys.time()
    amostra <- rgengamma_ar(n_j, par_i$delta, par_i$rho, par_i$alpha)
    end_time <- Sys.time()
    
    tempos_matriz[i, j] <- round(as.numeric(difftime(end_time, start_time, units = "mins")), 4)
    
    # Gráficos 1x2
    par(mfrow = c(1, 2))
    hist(amostra, breaks = 30, probability = TRUE, 
         main = paste("Cenário", i, "| N =", n_j), col = "gray90")
    curve(dgengamma(x, par_i$delta, par_i$rho, par_i$alpha), add = TRUE, col = "red", lwd = 2)
    
    plot(ecdf(amostra), main = "CDF Empírica vs Analítica")
    curve(pgengamma(x, par_i$delta, par_i$rho, par_i$alpha), add = TRUE, col = "blue", lwd = 2, lty = 2)
    
    # Testes de Aderência
    ks_p <- ks.test(amostra, "pgengamma", delta=par_i$delta, rho=par_i$rho, alpha=par_i$alpha)$p.value
    
    # Para N muito grandes, AD e CvM podem ser computacionalmente caros, mas incluídos conforme pedido
    ad_p <- ADGofTest::ad.test(amostra, pgengamma, delta=par_i$delta, rho=par_i$rho, alpha=par_i$alpha)$p.value
    cvm_p <- goftest::cvm.test(amostra, "pgengamma", delta=par_i$delta, rho=par_i$rho, alpha=par_i$alpha)$p.value
    
    # Formatação da linha de p-valores
    cat(sprintf("Cenário %d -> KS: %.4f | AD: %.4f | CvM: %.4f\n", 
                i, ks_p, ad_p, cvm_p))
  }
}

# -------------------------------------------------------------------------
# 5. Tabela de Tempos Final
# -------------------------------------------------------------------------
cat("\n\nTABELA COMPARATIVA DE TEMPOS (MINUTOS):\n")
tabela_tempos <- data.frame(Cenário = 1:6, tempos_matriz)
print(kable(tabela_tempos, format = "markdown"))