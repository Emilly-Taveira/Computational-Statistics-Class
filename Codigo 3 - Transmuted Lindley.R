# -------------------------------------------------------------------------
# 1. Bibliotecas e Funções Base
# -------------------------------------------------------------------------
if(!require(ADGofTest)) install.packages("ADGofTest")
if(!require(goftest)) install.packages("goftest")
if(!require(knitr)) install.packages("knitr")

library(ADGofTest)
library(goftest)
library(knitr)

# Função Densidade de Probabilidade (PDF)
dtlindley <- function(x, theta, lambda) {
  base_term <- ((theta + 1 + theta * x) / (theta + 1)) * exp(-theta * x)
  
  term1 <- (theta^2 / (theta + 1)) * (1 + x) * exp(-theta * x)
  term2 <- 1 - lambda + 2 * lambda * base_term
  
  return(term1 * term2)
}

# Função Distribuição Acumulada (CDF)
ptlindley <- function(x, theta, lambda) {
  base_term <- ((theta + 1 + theta * x) / (theta + 1)) * exp(-theta * x)
  
  cdf_val <- (1 - base_term) * (1 + lambda * base_term)
  return(cdf_val)
}

# -------------------------------------------------------------------------
# 2. FUNÇÃO QUANTIL (MÉTODO DA TRANSFORMAÇÃO INVERSA NUMÉRICA)
# -------------------------------------------------------------------------


qtlindley <- function(p, theta, lambda, lower = 1e-6, upper = 100) {
  sapply(p, function(u) {
    # Tratamento de extremos para evitar erros
    if (u <= 0) return(0)
    if (u >= 1) return(Inf)
    
    # Encontra a raiz da equação F(x) - u = 0
    root <- uniroot(function(x) ptlindley(x, theta, lambda) - u,
                    interval = c(lower, upper),
                    extendInt = "yes")$root
    return(root)
  })
}

# Gerador de números pseudoaleatórios
rtlindley <- function(n, theta, lambda) {
  u <- runif(n)
  return(qtlindley(u, theta, lambda))
}

# -------------------------------------------------------------------------
# 3. CONFIGURAÇÃO DOS CENÁRIOS
# -------------------------------------------------------------------------

# Definição dos cenários
cenarios <- data.frame(
  Cenario = 1:6,
  theta = c(1, 1, 1, 2, 0.5, 1.5),
  lambda = c(0, 0.8, -0.8, 0.5, -0.5, 1)
)

# Tamanhos de amostras
tamanhos_n <- c(10, 100, 1000, 10000, 100000)

# Matriz para armazenar o tempo de execução
tempos_execucao <- matrix(0, nrow = nrow(cenarios), ncol = length(tamanhos_n))
colnames(tempos_execucao) <- paste0("N_", tamanhos_n)
rownames(tempos_execucao) <- paste0("Cenário ", cenarios$Cenario)

# Lista para armazenar o p-valor dos testes de aderência
resultados_gof <- list()

# -------------------------------------------------------------------------
# 4. EXECUÇÃO, GRÁFICOS E TESTES DE ADERÊNCIA
# -------------------------------------------------------------------------


# Loop principal sobre os cenários e tamanhos de amostra
for (i in 1:nrow(cenarios)) {
  
  theta_atual <- cenarios$theta[i]
  lambda_atual <- cenarios$lambda[i]
  
  cat("\n======================================================\n")
  cat(sprintf("Iniciando Cenário %d (theta = %.1f, lambda = %.1f)", 
              cenarios$Cenario[i], theta_atual, lambda_atual))
  cat("\n======================================================\n")
  
  for (j in 1:length(tamanhos_n)) {
    n_atual <- tamanhos_n[j]
    
    # Início da contagem de tempo
    t_inicio <- Sys.time()
    
    # 1. Geração dos dados
    amostra <- rtlindley(n_atual, theta_atual, lambda_atual)
    
    # Fim da contagem de tempo
    t_fim <- Sys.time()
    tempo_minutos <- as.numeric(difftime(t_fim, t_inicio, units = "mins"))
    tempos_execucao[i, j] <- tempo_minutos
    
    # 2. Testes de Aderência
    ks_res <- suppressWarnings(ks.test(amostra, "ptlindley", theta = theta_atual, lambda = lambda_atual))
    ad_res <- ad.test(amostra, ptlindley, theta = theta_atual, lambda = lambda_atual)
    cvm_res <- cvm.test(amostra, "ptlindley", theta = theta_atual, lambda = lambda_atual)
    
    # Armazenando p-valores no log
    log_name <- paste0("Cen", i, "_N", n_atual)
    resultados_gof[[log_name]] <- c(KS_p = ks_res$p.value, 
                                    AD_p = ad_res$p.value, 
                                    CvM_p = cvm_res$p.value)
    
    # 3. Ilustrações Gráficas
    # Divide a tela em 1 linha e 2 colunas para cada combinação (Histograma + CDF)
    par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
    
    # --- Gráfico 1: Histograma com Densidade Teórica ---
    hist(amostra, breaks = "FD", freq = FALSE, 
         main = "Histograma vs Densidade", 
         xlab = "x", ylab = "Densidade", col = "lightblue", border = "white")
    
    # Eixo X para curva teórica
    x_grid <- seq(min(amostra), max(amostra), length.out = 500)
    lines(x_grid, dtlindley(x_grid, theta_atual, lambda_atual), 
          col = "red", lwd = 2)
    
    # --- Gráfico 2: CDF Empírica vs CDF Analítica ---
    plot(ecdf(amostra), main = "CDF Empírica vs Analítica", 
         xlab = "x", ylab = "F(x)", col = "blue", verticals = TRUE, do.points = FALSE)
    
    lines(x_grid, ptlindley(x_grid, theta_atual, lambda_atual), 
          col = "red", lwd = 2, lty = 2)
    legend("bottomright", legend=c("Empírica", "Analítica"), 
           col=c("blue", "red"), lty=c(1, 2), lwd=2, bty="n")
    
    # Título principal agrupado
    mtext(sprintf("Cenário %d | N = %d", cenarios$Cenario[i], n_atual), 
          outer = TRUE, cex = 1.2, font = 2)
    
    # Pausa pequena para renderização suave visual
    Sys.sleep(0.1) 
  }
}
# Reset da janela gráfica
par(mfrow = c(1, 1))

# -------------------------------------------------------------------------
# 5. TABELA DE TEMPOS E RESULTADOS GERAIS
# -------------------------------------------------------------------------


# 1. Exibição da Tabela de Tempos
tempos_formatados <- as.data.frame(matrix(paste(round(tempos_execucao, 4), "minutos"), 
                                          nrow = nrow(tempos_execucao)))
colnames(tempos_formatados) <- paste("N =", tamanhos_n)
tempos_formatados <- cbind(Cenario = 1:6, tempos_formatados)

cat("\n======================================================\n")
cat("          TABELA COMPARATIVA DE TEMPOS DE EXECUÇÃO          \n")
cat("======================================================\n\n")

print(kable(tempos_formatados, format = "markdown", align = "c"))

# 2. Loop para exibir as tabelas de p-valores para cada N
for (n_atual in tamanhos_n) {
  cat("\n======================================================\n")
  cat(sprintf("      AMOSTRA DE P-VALORES DOS TESTES (N=%s)      ", 
              format(n_atual, scientific = FALSE, big.mark = ".")))
  cat("\n======================================================\n")
  
  for (i in 1:nrow(cenarios)) {
    chave <- paste0("Cen", i, "_N", n_atual)
    
    pval <- resultados_gof[[chave]]
    
    cat(sprintf("Cenário %d -> KS: %.4f | AD: %.4f | CvM: %.4f\n", 
                i, pval["KS_p"], pval["AD_p"], pval["CvM_p"]))
  }
}