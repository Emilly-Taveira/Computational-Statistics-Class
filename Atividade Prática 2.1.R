# -------------------------------------------------------------------------
# 1. Definição da Função de Bisseção
# -------------------------------------------------------------------------
bissecao_emv <- function(h, a, b, tol = 1e-6, max.iter = 100) {
  # Verifica se a raiz está no intervalo
  if (h(a) * h(b) >= 0) {
    stop("O sinal de h(a) e h(b) deve ser oposto. Escolha outro intervalo [a, b].")
  }
  
  iter <- 0
  raiz <- a
  
  while ((b - a) / 2 > tol && iter < max.iter) {
    iter <- iter + 1
    raiz <- (a + b) / 2
    
    # Se a raiz for exata ou o intervalo for menor que a tolerância, para
    if (h(raiz) == 0) break
    
    # Decide qual subintervalo manter
    if (h(a) * h(raiz) < 0) {
      b <- raiz
    } else {
      a <- raiz
    }
  }
  
  return(list(raiz = raiz, iteracoes = iter))
}

# -------------------------------------------------------------------------
# 2. Configuração do Cenário Inicial (n = 50, lambda = 2.0)
# -------------------------------------------------------------------------
set.seed(123) # Para reprodutibilidade
n_inicial <- 50
lambda_real <- 2.0
amostra <- rexp(n_inicial, rate = lambda_real)

# Média amostral e EMV Analítico
x_barra <- mean(amostra)
emv_analitico <- 1 / x_barra

# Definição da função objetivo h(lambda) = n/lambda - sum(xi)
h_lambda <- function(lambda) {
  return(n_inicial / lambda - sum(amostra))
}

# -------------------------------------------------------------------------
# 3. Execução do Método e Gráfico
# -------------------------------------------------------------------------
# Escolha do intervalo baseada na análise visual (0 < lambda <= 5)
intervalo <- c(0.1, 5) 
resultado <- bissecao_emv(h_lambda, a = intervalo[1], b = intervalo[2])

# Plotagem da função h(lambda)
curve(h_lambda, from = 0.5, to = 5, col = "blue", lwd = 2,
      main = "Função Objetivo h(lambda) e Raiz Encontrada",
      xlab = expression(lambda), ylab = expression(h(lambda)))
abline(h = 0, lty = 2) # Linha do zero
points(resultado$raiz, 0, col = "red", pch = 19, cex = 1.5)
legend("topright", legend = c("h(lambda)", "EMV (Bisseção)"), 
       col = c("blue", "red"), pch = c(NA, 19), lty = c(1, NA))

# -------------------------------------------------------------------------
# 4. Comparação para diferentes tamanhos amostrais (n)
# -------------------------------------------------------------------------
tamanhos <- c(10, 30, 50, 100, 500)
tabela_resultados <- data.frame()

for (n in tamanhos) {
  dados_n <- rexp(n, rate = lambda_real)
  sum_x <- sum(dados_n)
  
  # Função h específica para esta amostra
  h_n <- function(l) n/l - sum_x
  
  # EMV Analítico
  emv_an <- 1 / mean(dados_n)
  
  # EMV Bisseção (ajustando intervalo se necessário)
  res_n <- bissecao_emv(h_n, 0.1, 10)
  
  tabela_resultados <- rbind(tabela_resultados, data.frame(
    n = n,
    EMV_Analitico = round(emv_an, 4),
    EMV_Bissecao = round(res_n$raiz, 4),
    Diferenca_Abs = abs(emv_an - res_n$raiz),
    Iteracoes = res_n$iteracoes
  ))
}

print(tabela_resultados)