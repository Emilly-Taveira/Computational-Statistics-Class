# ==============================================================================
# DISCIPLINA: Simulação Estatística
# OBJETIVO: Estimação de Máxima Verossimilhança para distribuição Weibull
#           com dados censurados à direita via Newton-Raphson-Fisher.
# ==============================================================================

# ------------------------------------------------------------------------------
# FUNÇÃO PRINCIPAL DE OTIMIZAÇÃO (NEWTON-RAPHSON)
# ------------------------------------------------------------------------------
weibull_mle_nrf <- function(t, delta, alpha_init = 1, beta_init = 1, 
                            tol = 1e-6, max_iter = 100, conf_level = 0.95) {
  
  # Inicialização dos parâmetros
  alpha <- alpha_init
  beta <- beta_init
  
  # Loop iterativo de Newton-Raphson
  for (k in 1:max_iter) {
    
    # Termos auxiliares para simplificar as contas
    z <- t / beta
    ln_z <- log(z)
    z_alpha <- z^alpha
    
    # Vetor Escore
    U_alpha <- sum(delta * (1/alpha - log(beta) + log(t)) - z_alpha * ln_z)
    U_beta  <- sum(-delta * (alpha/beta) + (alpha/beta) * z_alpha)
    
    U <- c(U_alpha, U_beta)
    
    # Matriz Hessiana
    H11 <- sum(-delta / (alpha^2) - z_alpha * (ln_z^2))
    H12 <- sum(-delta / beta + (1/beta) * z_alpha * (1 + alpha * ln_z))
    H22 <- sum(delta * alpha / (beta^2) - (alpha * (alpha + 1) / (beta^2)) * z_alpha)
    
    # Construção da matriz Hessiana
    H <- matrix(c(H11, H12, 
                  H12, H22), nrow = 2, ncol = 2)
    
    # Atualização dos Parâmetros
    passo <- solve(H, U)
    
    alpha_new <- alpha - passo[1]
    beta_new  <- beta - passo[2]
    
    
    # Critério de Parada
    erro_max <- max(abs(c(alpha_new - alpha, beta_new - beta)))
    
    alpha <- alpha_new
    beta  <- beta_new
    
    if (erro_max < tol) {
      cat("Convergência atingida na iteração:", k, "\n")
      break
    }
    
    if (k == max_iter) {
      warning("O algoritmo atingiu o número máximo de iterações sem convergir.")
    }
  }
  
  # CÁLCULO DA VARIÂNCIA E INTERVALOS DE CONFIANÇA
  
  # Recalculando o Hessiano no ponto de convergência
  z <- t / beta
  ln_z <- log(z)
  z_alpha <- z^alpha
  
  H11_final <- sum(-delta / (alpha^2) - z_alpha * (ln_z^2))
  H12_final <- sum(-delta / beta + (1/beta) * z_alpha * (1 + alpha * ln_z))
  H22_final <- sum(delta * alpha / (beta^2) - (alpha * (alpha + 1) / (beta^2)) * z_alpha)
  
  H_final <- matrix(c(H11_final, H12_final, 
                      H12_final, H22_final), nrow = 2, ncol = 2)
  
  # A Matriz de Informação de Fisher Observada é o negativo do Hessiano
  J_obs <- -H_final
  
  # A Matriz de Variância-Covariância Assintótica é a inversa da Informação de Fisher
  Var_Cov <- solve(J_obs)
  
  # Variâncias individuais
  var_alpha <- Var_Cov[1, 1]
  var_beta  <- Var_Cov[2, 2]
  
  # Quantil da Normal Padrão
  z_crit <- qnorm(1 - (1 - conf_level)/2)
  
  # Erros Padrão
  se_alpha <- sqrt(var_alpha)
  se_beta  <- sqrt(var_beta)
  
  # Intervalos de Confiança
  ic_alpha <- c(Limite_Inf = alpha - z_crit * se_alpha, Limite_Sup = alpha + z_crit * se_alpha)
  ic_beta  <- c(Limite_Inf = beta - z_crit * se_beta, Limite_Sup = beta + z_crit * se_beta)
  
  # RESULTADOS
  resultados <- list(
    Estimativas = c(Alpha = alpha, Beta = beta),
    Variancia_Estimadores = c(Var_Alpha = var_alpha, Var_Beta = var_beta),
    Matriz_Covariancia = Var_Cov,
    IC_Alpha = ic_alpha,
    IC_Beta = ic_beta,
    Iteracoes = k
  )
  
  return(resultados)
}

# ==============================================================================
# APLICAÇÃO DO ALGORITMO
# ==============================================================================

# Simulando um conjunto de dados para demonstrar o funcionamento
set.seed(2026) # Semente para reprodutibilidade

n <- 150
# Tempos de falha verdadeiros (Weibull com shape=2.5, scale=10)
tempos_falha <- rweibull(n, shape = 2.5, scale = 10)

# Tempos de censura 
tempos_censura <- rexp(n, rate = 0.05)

# Tempo observado 
t_observado <- pmin(tempos_falha, tempos_censura)

# Indicadora de falha
delta_observado <- as.numeric(tempos_falha <= tempos_censura)

cat("\nProporção de falhas observadas:", mean(delta_observado), "\n")

# Rodando o algoritmo de Newton-Raphson-Fisher
resultados_nrf <- weibull_mle_nrf(t = t_observado, 
                                  delta = delta_observado, 
                                  alpha_init = 1.0,  # Chutes iniciais
                                  beta_init = mean(t_observado), 
                                  tol = 1e-6, 
                                  max_iter = 100)

# Resultados
cat("\n--- RESULTADOS: MÉTODO NEWTON-RAPHSON-FISHER ---\n")
cat(sprintf("Estimador Alpha (Forma): %.4f\n", resultados_nrf$Estimativas["Alpha"]))
cat(sprintf("Variância de Alpha:      %.4f\n", resultados_nrf$Variancia_Estimadores["Var_Alpha"]))
cat(sprintf("IC de Alpha (95%%):       [%.4f ; %.4f]\n\n", resultados_nrf$IC_Alpha[1], resultados_nrf$IC_Alpha[2]))

cat(sprintf("Estimador Beta (Escala): %.4f\n", resultados_nrf$Estimativas["Beta"]))
cat(sprintf("Variância de Beta:       %.4f\n", resultados_nrf$Variancia_Estimadores["Var_Beta"]))
cat(sprintf("IC de Beta (95%%):        [%.4f ; %.4f]\n", resultados_nrf$IC_Beta[1], resultados_nrf$IC_Beta[2]))

# ==============================================================================
# GRÁFICO KAPLAN-MEIER E SOBREVIVÊNCIA WEIBULL
# ==============================================================================

# Ordenando os dados pelos tempos observados
ordem <- order(t_observado)
t_ord <- t_observado[ordem]
delta_ord <- delta_observado[ordem]

# Encontrando os tempos únicos onde ocorreram falhas (delta == 1)
tempos_falha_unicos <- unique(t_ord[delta_ord == 1])

# Vetores para armazenar a probabilidade de sobrevivência e o tempo
S_km <- numeric(length(tempos_falha_unicos))
prob_sobrevivencia_atual <- 1

# Cálculo iterativo do Kaplan-Meier: S(t) = S(t-1) * (1 - d_j / n_j)
for (i in 1:length(tempos_falha_unicos)) {
  tj <- tempos_falha_unicos[i]
  
  # d_j: Número de falhas no tempo tj
  dj <- sum(t_ord == tj & delta_ord == 1)
  
  # n_j: Número de indivíduos em risco (tempo observado >= tj)
  nj <- sum(t_ord >= tj)
  
  # Atualizando a probabilidade
  prob_sobrevivencia_atual <- prob_sobrevivencia_atual * (1 - dj / nj)
  S_km[i] <- prob_sobrevivencia_atual
}

# Adicionando o tempo t=0 com S(0)=1 para o gráfico iniciar corretamente
tempos_plot <- c(0, tempos_falha_unicos)
surv_plot <- c(1, S_km)

## Curva de Sobrevivência da Weibull Ajustada
alpha_estimado <- resultados_nrf$Estimativas["Alpha"]
beta_estimado  <- resultados_nrf$Estimativas["Beta"]

# Criando um grid contínuo de tempos para traçar uma curva suave
t_grid <- seq(0, max(t_observado), length.out = 500)

# Função de Sobrevivência da Weibull
surv_weibull <- exp(-(t_grid / beta_estimado)^alpha_estimado)

## Plotagem do Gráfico

# Plotando o Kaplan-Meier empírico em formato de escada
plot(tempos_plot, surv_plot, type = "s", lwd = 2, col = "black",
     xlab = "Tempo", ylab = "Probabilidade de Sobrevivência S(t)",
     main = "Kaplan-Meier Empírico vs. Weibull Ajustada",
     ylim = c(0, 1), xlim = c(0, max(t_observado)))

# Curva da Weibull paramétrica
lines(t_grid, surv_weibull, lwd = 2, col = "red", lty = 2)

# Legenda 
legend("topright", 
       legend = c("Empírica (Kaplan-Meier)", "Paramétrica (Weibull Ajustada)"),
       col = c("black", "red"), lwd = 2, lty = c(1, 2), bty = "n")