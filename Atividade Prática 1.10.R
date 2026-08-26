# =================================================================
# Atividade Prática 1.10: Distribuição Exponencial 
# =================================================================

install.packages("microbenchmark")
library(microbenchmark)


# Função Analítica
rexp_analitico <- function(n, lambda) {
  u <- runif(n, min = 0, max = 1)
  quantil <- -(1 / lambda) * log(1 - u)
  
  return(quantil)
}

# Função Numérica
rexp_numerico <- function(n, lambda) {
  u_vetor <- runif(n, min = 0, max = 1)
  x_num <- sapply(u_vetor, function(u) {
    raiz <- uniroot(function(x) pexp(x, rate = lambda) - u, 
                    interval = c(0, 100))
    return(raiz$root)
  })
  
  return(x_num)
}

# Benchmarking e Tabela

lambda <- 0.5
n <- 1000
B <- 100

# Executando B = 100 replicações para cada método
cat("Executando benchmarking...\n")
resultado_bench <- microbenchmark(
  Metodo_Analitico = rexp_analitico(n, lambda),
  Metodo_Numerico  = rexp_numerico(n, lambda),
  times = B,
  unit = "s"
)

tempos_analitico <- resultado_bench$time[resultado_bench$expr == "Metodo_Analitico"] / 1e9
tempos_numerico  <- resultado_bench$time[resultado_bench$expr == "Metodo_Numerico"] / 1e9

# Tabela de documentação
tabela_resultados <- data.frame(
  Metodo = c("Analítico", "Numérico"),
  Tempo_Medio_seg = c(mean(tempos_analitico), mean(tempos_numerico)),
  Desvio_Padrao_seg = c(sd(tempos_analitico), sd(tempos_numerico)),
  Tempo_Minimo_seg = c(min(tempos_analitico), min(tempos_numerico)),
  Tempo_Maximo_seg = c(max(tempos_analitico), max(tempos_numerico))
)

# Diferença percentual entre os tempos médios
# Fórmula: ((Maior - Menor) / Menor) * 100
diff_percentual <- ((tabela_resultados$Tempo_Medio_seg[2] - tabela_resultados$Tempo_Medio_seg[1]) / 
                      tabela_resultados$Tempo_Medio_seg[1]) * 100

cat("\n--- Tabela de Resultados do Benchmarking ---\n")
print(tabela_resultados)
cat(sprintf("\nDiferença percentual de tempo médio: O método numérico é %.2f%% mais lento.\n\n", diff_percentual))

# Gráficos
set.seed(42)
amostra_analitica <- rexp_analitico(n, lambda)

set.seed(42)
amostra_numerica <- rexp_numerico(n, lambda)

par(mfrow = c(1, 2))

# 1. Histograma + Curva Teórica
hist(amostra_analitica, probability = TRUE, col = 'skyblue', 
     main = "Qualidade da Simulação\n(Método Analítico)", 
     xlab = "Valores Gerados (x)", ylab = "Densidade")
curve(dexp(x, rate = lambda), col = 'red', lwd = 2, add = TRUE)
legend("topright", legend = "Densidade Teórica", col = "red", lwd = 2, bty = "n")

# 2. Gráfico de Dispersão e Validação
plot(amostra_analitica, amostra_numerica, 
     main = "Validação dos Métodos\nAnalítico vs Numérico",
     xlab = "Valores (Método Analítico)", 
     ylab = "Valores (Método Numérico)",
     pch = 16, col = "darkgray")
abline(a = 0, b = 1, col = "blue", lwd = 2) # Reta identidade
legend("topleft", legend = "Reta Identidade (y=x)", col = "blue", lwd = 2, bty = "n")

