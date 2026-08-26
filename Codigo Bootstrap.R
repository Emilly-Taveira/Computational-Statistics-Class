# ==============================================================================
# SCRIPT 1: PARTE 1 - MÉTODO BOOTSTRAP PARA REGRESSÃO POLINOMIAL
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Configuração Inicial e Simulação dos Dados
# ------------------------------------------------------------------------------
set.seed(241251419) # Semente usando a minha matrícula
N <- 200000        # Tamanho da amostra

cat("Simulando os dados...\n")
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

# Construindo a Matriz X com Intercepto
X_mat <- cbind(1, dados_scaled, dados_quad) 
colnames(X_mat)[1] <- "Intercepto"
p <- ncol(X_mat) # Número de parâmetros (21)

# Variável Resposta (Y)
beta <- c(0.25, -0.06, 1.1, -0.28, 0.00012, -0.22, 0.004, 0.09, 0.025, -0.018,
          -0.45, 0.008, 1.2, 5.8, 0.000002, 1.1, 0.00012, 2.0, -0.012, -0.22)
beta_0 <- 0.6
beta_real <- c(beta_0, beta)

erro <- rnorm(N, mean = 0, sd = 4)
Y <- as.vector(X_mat %*% beta_real) + erro

# ------------------------------------------------------------------------------
# 2. Estimação do Modelo Original (MQO) e Erro-Padrão
# ------------------------------------------------------------------------------
XtX_inv <- solve(crossprod(X_mat))
beta_hat <- as.vector(XtX_inv %*% crossprod(X_mat, Y))

# Erro padrão original
e_orig <- Y - as.vector(X_mat %*% beta_hat)
sigma2_orig <- sum(e_orig^2) / (N - p)
se_beta_hat <- sqrt(diag(XtX_inv) * sigma2_orig)

# ------------------------------------------------------------------------------
# 3. Execução do Método Bootstrap
# ------------------------------------------------------------------------------
B <- 2000 # Número de amostras Bootstrap (Ajuste para 2000 se quiser testar rápido)

# Matrizes para armazenar os resultados das iterações
boot_betas <- matrix(NA, nrow = B, ncol = p)
boot_t_stats <- matrix(NA, nrow = B, ncol = p)

cat("Iniciando as", B, "iterações do Bootstrap...\n")
pb <- txtProgressBar(min = 0, max = B, style = 3) # Barra de acompanhamento

for (b in 1:B) {
  # Amostragem com reposição
  indices <- sample(1:N, size = N, replace = TRUE)
  X_star <- X_mat[indices, ]
  Y_star <- Y[indices]
  
  # Reestimação por MQO para a amostra Bootstrap
  XtX_inv_star <- solve(crossprod(X_star))
  beta_star <- as.vector(XtX_inv_star %*% crossprod(X_star, Y_star))
  boot_betas[b, ] <- beta_star
  
  # Cálculo da estatística-t da amostra Bootstrap
  e_star <- Y_star - as.vector(X_star %*% beta_star)
  sigma2_star <- sum(e_star^2) / (N - p)
  se_beta_star <- sqrt(pmax(diag(XtX_inv_star) * sigma2_star, 0))
  
  boot_t_stats[b, ] <- (beta_star - beta_hat) / se_beta_star
  
  # Atualiza a barra a cada 1000 iterações para não sobrecarregar o processador
  if(b %% 1000 == 0) setTxtProgressBar(pb, b)
}
setTxtProgressBar(pb, B)
close(pb)

# ------------------------------------------------------------------------------
# 4. Cálculo das Estimativas Bootstrap
# ------------------------------------------------------------------------------
beta_boot_mean <- colMeans(boot_betas)
se_boot <- apply(boot_betas, 2, sd)

# ------------------------------------------------------------------------------
# 5. Construção dos 4 Tipos de Intervalos de Confiança (95%)
# ------------------------------------------------------------------------------
alpha <- 0.05
resultados_ICs <- list()

for (j in 1:p) {
  # 1. Percentil: Extração direta dos quantis
  q_inf_perc <- quantile(boot_betas[, j], alpha / 2)
  q_sup_perc <- quantile(boot_betas[, j], 1 - alpha / 2)
  
  # 2. Aproximação Normal: Baseado no Erro Padrão Bootstrap
  z_crit <- qnorm(1 - alpha / 2)
  ic_inf_norm <- beta_hat[j] - z_crit * se_boot[j]
  ic_sup_norm <- beta_hat[j] + z_crit * se_boot[j]
  
  # 3. Percentil Reverso (Básico): Usa a assimetria espelhada
  ic_inf_rev <- 2 * beta_hat[j] - q_sup_perc
  ic_sup_rev <- 2 * beta_hat[j] - q_inf_perc
  
  # 4. Método t-Student: Usando quantis da estatística t empírica
  t_inf <- quantile(boot_t_stats[, j], alpha / 2)
  t_sup <- quantile(boot_t_stats[, j], 1 - alpha / 2)
  ic_inf_t <- beta_hat[j] - t_sup * se_beta_hat[j]
  ic_sup_t <- beta_hat[j] - t_inf * se_beta_hat[j]
  
  resultados_ICs[[colnames(X_mat)[j]]] <- data.frame(
    Parametro = colnames(X_mat)[j],
    Estimativa_Original = round(beta_hat[j], 6),
    Estimativa_Bootstrap = round(beta_boot_mean[j], 6),
    Percentil_2.5 = round(q_inf_perc, 4), Percentil_97.5 = round(q_sup_perc, 4),
    Normal_Inf = round(ic_inf_norm, 4), Normal_Sup = round(ic_sup_norm, 4),
    Reverso_Inf = round(ic_inf_rev, 4), Reverso_Sup = round(ic_sup_rev, 4),
    tStudent_Inf = round(ic_inf_t, 4), tStudent_Sup = round(ic_sup_t, 4),
    row.names = NULL
  )
}

df_resultados_Bootstrap <- do.call(rbind, resultados_ICs)
cat("\n\n=============== RESULTADOS BOOTSTRAP ===============\n")
print(df_resultados_Bootstrap)