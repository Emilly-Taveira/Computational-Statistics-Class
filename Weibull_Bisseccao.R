# ==============================================================================
# DISCIPLINA: Simulação Estatística
# OBJETIVO: Estimação de Máxima Verossimilhança para distribuição Weibull
#           com dados censurados à direita via Método da Bissecção.
# ==============================================================================

# ------------------------------------------------------------------------------
# FUNÇÃO PRINCIPAL DA BISSECÇÃO
# ------------------------------------------------------------------------------
weibull_mle_bisseccao <- function(t, delta, intervalo_alpha = c(0.1, 10), 
                                  tol = 1e-6, max_iter = 100, conf_level = 0.95) {
  
  # Número de falhas observadas
  r <- sum(delta)
  if (r == 0) stop("Erro: Nenhuma falha observada no conjunto de dados.")
  
  # Passo 1: Definição da Função Escore para Alpha
  g_alpha <- function(alpha) {
    termo1 <- r / alpha
    termo2 <- sum(delta * log(t))
    termo3 <- (r * sum((t^alpha) * log(t))) / sum(t^alpha)
    
    return(termo1 + termo2 - termo3)
  }
  
  # Passo 2: Algoritmo da Bissecção
  a <- intervalo_alpha[1]
  b <- intervalo_alpha[2]
  
  fa <- g_alpha(a)
  fb <- g_alpha(b)
  
  # Verifica se existe mudança de sinal no intervalo inicial
  if (fa * fb > 0) {
    stop("O intervalo inicial de alpha não contém uma raiz óbvia (f(a) e f(b) têm o mesmo sinal). Amplie o intervalo.")
  }
  
  iter_convergiu <- max_iter
  
  # Loop iterativo da Bissecção
  for (k in 1:max_iter) {
    c_mid <- (a + b) / 2
    fc <- g_alpha(c_mid)
    
    # Critério de parada: raiz encontrada ou intervalo suficientemente pequeno
    if (abs(fc) < tol || (b - a) / 2 < tol) {
      alpha_hat <- c_mid
      iter_convergiu <- k
      cat("Convergência atingida na iteração:", k, "\n")
      break
    }
    
    # Atualização do intervalo
    if (fa * fc < 0) {
      b <- c_mid
      fb <- fc
    } else {
      a <- c_mid
      fa <- fc
    }
    
    if (k == max_iter) {
      alpha_hat <- (a + b) / 2
      warning("O algoritmo da bissecção atingiu o número máximo de iterações.")
    }
  }
  
  # Passo 3: Cálculo do Estimador de Beta
  
  beta_hat <- (sum(t^alpha_hat) / r)^(1 / alpha_hat)
  
  # CÁLCULO DA VARIÂNCIA E INTERVALOS DE CONFIANÇA
  
  # Calculando a Matriz Hessiana avaliada nos estimadores encontrados
  alpha <- alpha_hat
  beta <- beta_hat
  
  z <- t / beta
  ln_z <- log(z)
  z_alpha <- z^alpha
  
  H11_final <- sum(-delta / (alpha^2) - z_alpha * (ln_z^2))
  H12_final <- sum(-delta / beta + (1/beta) * z_alpha * (1 + alpha * ln_z))
  H22_final <- sum(delta * alpha / (beta^2) - (alpha * (alpha + 1) / (beta^2)) * z_alpha)
  
  H_final <- matrix(c(H11_final, H12_final, 
                      H12_final, H22_final), nrow = 2, ncol = 2)
  
  # Matriz de Informação de Fisher Observada (negativo do Hessiano)
  J_obs <- -H_final
  
  # Matriz de Variância-Covariância Assintótica
  Var_Cov <- solve(J_obs)
  
  var_alpha <- Var_Cov[1, 1]
  var_beta  <- Var_Cov[2, 2]
  
  # Intervalos de Confiança
  z_crit <- qnorm(1 - (1 - conf_level)/2)
  se_alpha <- sqrt(var_alpha)
  se_beta  <- sqrt(var_beta)
  
  ic_alpha <- c(Limite_Inf = alpha - z_crit * se_alpha, Limite_Sup = alpha + z_crit * se_alpha)
  ic_beta  <- c(Limite_Inf = beta - z_crit * se_beta, Limite_Sup = beta + z_crit * se_beta)
  
  
  ## RESULTADOS
  
  resultados <- list(
    Estimativas = c(Alpha = alpha_hat, Beta = beta_hat),
    Variancia_Estimadores = c(Var_Alpha = var_alpha, Var_Beta = var_beta),
    Matriz_Covariancia = Var_Cov,
    IC_Alpha = ic_alpha,
    IC_Beta = ic_beta,
    Iteracoes = iter_convergiu
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

# Rodando o algoritmo da Bissecção
resultados_bisseccao <- weibull_mle_bisseccao(t = t_observado, 
                                              delta = delta_observado, 
                                              intervalo_alpha = c(0.1, 10), 
                                              tol = 1e-6, 
                                              max_iter = 100)

# Exibindo os resultados
cat("\n--- RESULTADOS: MÉTODO DA BISSECÇÃO ---\n")
cat(sprintf("Estimador Alpha (Forma): %.4f\n", resultados_bisseccao$Estimativas["Alpha"]))
cat(sprintf("Variância de Alpha:      %.4f\n", resultados_bisseccao$Variancia_Estimadores["Var_Alpha"]))
cat(sprintf("IC de Alpha (95%%):       [%.4f ; %.4f]\n\n", resultados_bisseccao$IC_Alpha[1], resultados_bisseccao$IC_Alpha[2]))

cat(sprintf("Estimador Beta (Escala): %.4f\n", resultados_bisseccao$Estimativas["Beta"]))
cat(sprintf("Variância de Beta:       %.4f\n", resultados_bisseccao$Variancia_Estimadores["Var_Beta"]))
cat(sprintf("IC de Beta (95%%):        [%.4f ; %.4f]\n", resultados_bisseccao$IC_Beta[1], resultados_bisseccao$IC_Beta[2]))

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
alpha_estimado <- resultados_bisseccao$Estimativas["Alpha"]
beta_estimado  <- resultados_bisseccao$Estimativas["Beta"]

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