# --- 1. Definição das Funções da Distribuição Burr XII ---

# Função de Distribuição Acumulada (f.d.a)
pburrxii <- function(x, c, k) {
  1 - (1 + x^c)^(-k)
}

# Função Densidade de Probabilidade (f.d.p)
dburrxii <- function(x, c, k) {
  c * k * x^(c - 1) * (1 + x^c)^(-(k + 1))
}

# Função Quantil (Transformação Inversa)
qburrxii <- function(u, c, k) {
  ((1 - u)^(-1/k) - 1)^(1/c)
}

# Gerador de amostras (rburrxii)
rburrxii <- function(n, c, k) {
  u <- runif(n)
  return(qburrxii(u, c, k))
}

# --- 2. Simulação Inicial (n = 1000, c = 2.0, k = 1.5) ---

set.seed(123) # Para reprodutibilidade
n <- 1000
c_param <- 2.0
k_param <- 1.5

amostra <- rburrxii(n, c_param, k_param)

# --- 3. Teste de Anderson-Darling ---

if (!require(ADGofTest)) install.packages("ADGofTest")
library(ADGofTest)

teste_ad <- ad.test(amostra, distr.fun = pburrxii, c = c_param, k = k_param)

cat("--- Resultados do Teste de Anderson-Darling ---\n")
cat("Estatística A²:", teste_ad$statistic, "\n")
cat("p-valor:", teste_ad$p.value, "\n")
cat("Conclusão:", ifelse(teste_ad$p.value > 0.05, 
                         "Não rejeita H0 (Amostra segue Burr XII)", 
                         "Rejeita H0"), "\n\n")

# --- 4. Visualizações Gráficas ---

par(mfrow = c(1, 2))

# Histograma vs Densidade Teórica
hist(amostra, breaks = 30, probability = TRUE, main = "Histograma vs Densidade",
     xlab = "x", col = "pink", border = "white")
curve(dburrxii(x, c_param, k_param), add = TRUE, col = "purple", lwd = 2)
legend("topright", legend = c("Teórica"), col = "purple", lwd = 2)

# FDA Empírica vs FDA Teórica
plot(ecdf(amostra), main = "FDA Empírica vs Teórica", xlab = "x", ylab = "F(x)")
curve(pburrxii(x, c_param, k_param), add = TRUE, col = "purple", lwd = 2)
legend("bottomright", legend = c("Empírica", "Teórica"), 
       col = c("black", "purple"), lwd = 1:2)

# --- 5. Replicação (B = 100) ---

B <- 100
p_valores <- numeric(B)

for (i in 1:B) {
  amostra_sim <- rburrxii(n, c_param, k_param)
  p_valores[i] <- ad.test(amostra_sim, distr.fun = pburrxii, c = c_param, k = k_param)$p.value
}

taxa_aceitacao <- mean(p_valores > 0.05)

cat("--- Resultados das B = 100 Réplicas ---\n")
cat("Taxa de aceitação (1 - alpha):", taxa_aceitacao, "\n")