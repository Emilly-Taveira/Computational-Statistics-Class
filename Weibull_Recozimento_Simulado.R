# ==============================================================================
# DISCIPLINA: Simulação Estatística
# OBJETIVO: Estimação de Máxima Verossimilhança para distribuição Weibull
#           com dados censurados à direita via Recozimento Simulado
# ==============================================================================

# ------------------------------------------------------------------------------
# FUNÇÃO OBJETIVO
# ------------------------------------------------------------------------------
neg_log_verossimilhanca <- function(par, t, delta) {
  alpha <- par[1]
  beta <- par[2]
  
  # Os parâmetros devem ser > 0
  if (alpha <= 0 || beta <= 0) {
    return(Inf) 
  }
  
  z <- t / beta
  ln_z <- log(z)
  z_alpha <- z^alpha
  
  # Log-verossimilhança original
  log_lik <- sum(delta * (log(alpha) - log(beta) + log(t)) - z_alpha * ln_z) # simplificação
  
  log_lik <- sum(delta * (log(alpha) - alpha * log(beta) + (alpha - 1) * log(t)) - (t / beta)^alpha)
  
  # Retorna a negativa, pois o Simulated Annealing vai MINIMIZAR
  return(-log_lik)
}

# ------------------------------------------------------------------------------
# FUNÇÃO PRINCIPAL (RECOZIMENTO SIMULADO)
# ------------------------------------------------------------------------------
weibull_mle_sann <- function(t, delta, par_init = c(1, 1), 
                             Temp_init = 100, resfriamento = 0.95, 
                             max_iter = 5000, conf_level = 0.95) {
  
  # Inicialização do estado e da energia
  estado_atual <- par_init
  energia_atual <- neg_log_verossimilhanca(estado_atual, t, delta)
  
  # Variáveis para guardar o melhor cenário encontrado
  melhor_estado <- estado_atual
  melhor_energia <- energia_atual
  
  Temp <- Temp_init
  
  # Loop iterativo do Recozimento Simulado
  for (k in 1:max_iter) {
    
    # Gerar um estado "vizinho" (perturbação aleatória normal)
    vizinho <- estado_atual + rnorm(2, mean = 0, sd = 0.1 * sqrt(Temp))
    
    # Calcular a energia do vizinho
    energia_vizinho <- neg_log_verossimilhanca(vizinho, t, delta)
    
    # Diferença de energia
    delta_E <- energia_vizinho - energia_atual
    
    # Critério de Aceitação
    if (delta_E < 0 || runif(1) < exp(-delta_E / Temp)) {
      estado_atual <- vizinho
      energia_atual <- energia_vizinho
      
      # Atualiza o melhor global encontrado até agora
      if (energia_atual < melhor_energia) {
        melhor_estado <- estado_atual
        melhor_energia <- energia_atual
      }
    }
    
    # Resfriamento do sistema (Geométrico)
    Temp <- Temp * resfriamento
  }
  
  alpha_hat <- melhor_estado[1]
  beta_hat  <- melhor_estado[2]
  
  # CÁLCULO DA VARIÂNCIA E INTERVALOS DE CONFIANÇA (VIA FISHER)
  # Utiliza-se o Hessiano avaliado no estimador global encontrado
  
  alpha <- alpha_hat
  beta <- beta_hat
  
  z <- t / beta
  ln_z <- log(z)
  z_alpha <- z^alpha
  
  H11 <- sum(-delta / (alpha^2) - z_alpha * (ln_z^2))
  H12 <- sum(-delta / beta + (1/beta) * z_alpha * (1 + alpha * ln_z))
  H22 <- sum(delta * alpha / (beta^2) - (alpha * (alpha + 1) / (beta^2)) * z_alpha)
  
  H_final <- matrix(c(H11, H12, 
                      H12, H22), nrow = 2, ncol = 2)
  
  # Informação de Fisher Observada e Matriz de Covariância
  J_obs <- -H_final
  Var_Cov <- solve(J_obs)
  
  var_alpha <- Var_Cov[1, 1]
  var_beta  <- Var_Cov[2, 2]
  
  # Erros Padrão e Quantil Normal
  se_alpha <- sqrt(var_alpha)
  se_beta  <- sqrt(var_beta)
  z_crit <- qnorm(1 - (1 - conf_level)/2)
  
  # Intervalos de Confiança
  ic_alpha <- c(Limite_Inf = alpha - z_crit * se_alpha, Limite_Sup = alpha + z_crit * se_alpha)
  ic_beta  <- c(Limite_Inf = beta - z_crit * se_beta, Limite_Sup = beta + z_crit * se_beta)
  
  # RESULTADOS
  resultados <- list(
    Estimativas = c(Alpha = alpha_hat, Beta = beta_hat),
    Variancia_Estimadores = c(Var_Alpha = var_alpha, Var_Beta = var_beta),
    Matriz_Covariancia = Var_Cov,
    IC_Alpha = ic_alpha,
    IC_Beta = ic_beta,
    Iteracoes = max_iter
  )
  
  return(resultados)
}

# ==============================================================================
# APLICAÇÃO DO ALGORITMO
# ==============================================================================

set.seed(2026) 

n <- 150
tempos_falha <- rweibull(n, shape = 2.5, scale = 10)
tempos_censura <- rexp(n, rate = 0.05)

t_observado <- pmin(tempos_falha, tempos_censura)
delta_observado <- as.numeric(tempos_falha <= tempos_censura)

# Rodando o algoritmo de Recozimento Simulado
resultados_sann <- weibull_mle_sann(t = t_observado, 
                                    delta = delta_observado, 
                                    par_init = c(1.0, mean(t_observado)), 
                                    Temp_init = 10,       # Temperatura inicial
                                    resfriamento = 0.99,  # Resfriamento 
                                    max_iter = 5000)

# Resultados
cat("\n--- RESULTADOS: RECOZIMENTO SIMULADO (SA) ---\n")
cat(sprintf("Estimador Alpha (Forma): %.4f\n", resultados_sann$Estimativas["Alpha"]))
cat(sprintf("Variância de Alpha:      %.4f\n", resultados_sann$Variancia_Estimadores["Var_Alpha"]))
cat(sprintf("IC de Alpha (95%%):       [%.4f ; %.4f]\n\n", resultados_sann$IC_Alpha[1], resultados_sann$IC_Alpha[2]))

cat(sprintf("Estimador Beta (Escala): %.4f\n", resultados_sann$Estimativas["Beta"]))
cat(sprintf("Variância de Beta:       %.4f\n", resultados_sann$Variancia_Estimadores["Var_Beta"]))
cat(sprintf("IC de Beta (95%%):        [%.4f ; %.4f]\n", resultados_sann$IC_Beta[1], resultados_sann$IC_Beta[2]))

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
alpha_estimado <- resultados_sann$Estimativas["Alpha"]
beta_estimado  <- resultados_sann$Estimativas["Beta"]

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