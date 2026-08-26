# ==============================================================================
# PARTE 2 - SCRIPT MÉTODO JACKKNIFE
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Configuração Inicial e Simulação dos Dados 
# ------------------------------------------------------------------------------
set.seed(241251419) # Semente com o RA

N <- 200000  

cat("Simulando os dados para N =", N, "...\n")
X1  <- abs(rnorm(N, mean = 0.15, sd = 0.04))
X2  <- rlnorm(N, meanlog = log(18), sdlog = 0.25)
X3  <- rnorm(N, mean = 0.003, sd = 0.012)
X4  <- rnorm(N, mean = 0.0007, sd = 0.008)
X5  <- rlnorm(N, meanlog = log(800), sdlog = 0.35)
X6  <- rnorm(N, mean = 0.04, sd = 0.004)
X7  <- runif(N, min = 5, max = 95)
X8  <- rnorm(N, mean = 0, sd = 0.4)
X9  <- rlnorm(N, meanlog = log(1.7), sdlog = 0.15)
X10 <- rlnorm(N, meanlog = log(2.5), sdlog = 0.25)

dados <- data.frame(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10)
dados_scaled <- scale(dados, center = TRUE, scale = TRUE)
dados_quad <- dados_scaled^2
colnames(dados_quad) <- paste0(colnames(dados_scaled), "_2")

X_mat <- cbind(1, dados_scaled, dados_quad) 
colnames(X_mat)[1] <- "Intercepto"
p <- ncol(X_mat)

beta <- c(0.25, -0.06, 1.1, -0.28, 0.00012, -0.22, 0.004, 0.09, 0.025, -0.018,
          -0.45, 0.008, 1.2, 5.8, 0.000002, 1.1, 0.00012, 2.0, -0.012, -0.22)
beta_0 <- 0.6
beta_real <- c(beta_0, beta)

erro <- rnorm(N, mean = 0, sd = 4)
Y <- as.vector(X_mat %*% beta_real) + erro

# ------------------------------------------------------------------------------
# 2. Estimação do Modelo Original (MQO)
# ------------------------------------------------------------------------------
cat("Calculando o modelo original...\n")
XtX_inv <- solve(crossprod(X_mat))
beta_hat <- as.vector(XtX_inv %*% crossprod(X_mat, Y))

# ------------------------------------------------------------------------------
# 3. Execução do Método Jackknife Clássico
# ------------------------------------------------------------------------------
jack_betas <- matrix(NA, nrow = N, ncol = p)
colnames(jack_betas) <- colnames(X_mat)

cat("Iniciando as", N, "reestimações do Jackknife (com exclusão sequencial)...\n")
pb <- txtProgressBar(min = 0, max = N, style = 3)

for(i in 1:N) {
  
  # (a) O modelo será reestimado n vezes, onde cada reestimativa será obtida 
  #     com a i-ésima observação retirada.
  
  # Exclui a linha i da matriz X e do vetor Y
  X_jack <- X_mat[-i, ]
  Y_jack <- Y[-i]
  
  # Recalcula as estimativas por MQO sem usar pacotes: beta = (X'X)^-1 X'Y
  XtX_inv_jack <- solve(crossprod(X_jack))
  beta_jack <- as.vector(XtX_inv_jack %*% crossprod(X_jack, Y_jack))
  
  # Armazena a estimativa do parâmetro theta_(-i)
  jack_betas[i, ] <- beta_jack
  
  # Atualiza a barra de progresso a cada 100 interações para não travar o console
  if(i %% 100 == 0) setTxtProgressBar(pb, i)
}
setTxtProgressBar(pb, N)
close(pb)

# ------------------------------------------------------------------------------
# 4. Determinação da Média e Erro-Padrão utilizando as fórmulas
# ------------------------------------------------------------------------------
cat("\nCalculando médias, erros-padrão e Intervalos de Confiança...\n")

# (b) Determinar a média dessas estimativas de acordo com a fórmula
beta_jack_mean <- colMeans(jack_betas)

# (c) Determinar o erro-padrão jackknife para cada parâmetro 

# Calculando a diferença ao quadrado: (theta_i - theta_(.))^2
desvios_quad <- sweep(jack_betas, 2, beta_jack_mean, "-")^2

# Aplicando a fórmula: sqrt( ((n-1)/n) * sum(...) )
ep_jack <- sqrt( ((N - 1) / N) * colSums(desvios_quad) )

# ------------------------------------------------------------------------------
# 5. ICs de 95% via Jackknife para os 21 Parâmetros
# ------------------------------------------------------------------------------
# (d) Construção dos intervalos de confiança de 95%
alpha <- 0.05
# Quantil da distribuição t-Student com (N - 1) graus de liberdade
t_crit <- qt(1 - alpha / 2, df = N - 1)

ic_inf_jack <- beta_jack_mean - t_crit * ep_jack
ic_sup_jack <- beta_jack_mean + t_crit * ep_jack

# Consolidando tudo num Data Frame
df_resultados_Jackknife <- data.frame(
  Parametro = colnames(X_mat),
  Estimativa_Original = round(beta_hat, 6),
  Media_Jackknife = round(beta_jack_mean, 6),
  Erro_Padrao_Jackknife = round(ep_jack, 4),
  IC_95_Inf = round(ic_inf_jack, 4),
  IC_95_Sup = round(ic_sup_jack, 4),
  row.names = NULL
)

cat("\n\n=============== RESULTADOS JACKKNIFE ===============\n")
print(df_resultados_Jackknife)