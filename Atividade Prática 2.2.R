# -------------------------------------------------------------------------
# 1. Definições das Funções (f, f' e f'')
# -------------------------------------------------------------------------
f <- function(x) x^4 - 8*x^3 + 18*x^2 - 11*x + 5
f_prime <- function(x) 4*x^3 - 24*x^2 + 36*x - 11
f_double_prime <- function(x) 12*x^2 - 48*x + 36

# -------------------------------------------------------------------------
# 2. Implementação do Método de Newton-Raphson
# -------------------------------------------------------------------------
newton_raphson <- function(f_p, f_pp, x0, tol = 1e-8, max.iter = 100) {
  x <- x0
  iter <- 0
  trajetoria <- x0 # Para visualização posterior
  
  repeat {
    iter <- iter + 1
    f_p_val <- f_p(x)
    f_pp_val <- f_pp(x)
    
    # Prevenção contra divisão por zero
    if (abs(f_pp_val) < 1e-10) {
      warning("Segunda derivada muito próxima de zero. O método pode falhar.")
      break
    }
    
    x_novo <- x - f_p_val / f_pp_val
    trajetoria <- c(trajetoria, x_novo)
    
    # Critério de parada baseado no valor absoluto da derivada
    if (abs(f_p(x_novo)) < tol || iter >= max.iter) {
      x <- x_novo
      break
    }
    x <- x_novo
  }
  
  return(list(raiz = x, f_val = f(x), iter = iter, trajetoria = trajetoria))
}

# -------------------------------------------------------------------------
# 3. Execução para os Valores Iniciais Propostos
# -------------------------------------------------------------------------
valores_iniciais <- c(0.0, 1.5, 3.0, 4.5)
tabela_resultados <- data.frame()
lista_trajetorias <- list()

for (x0 in valores_iniciais) {
  res <- newton_raphson(f_prime, f_double_prime, x0)
  
  # Classificação do ponto crítico
  f_pp_final <- f_double_prime(res$raiz)
  classificacao <- ifelse(f_pp_final > 1e-5, "Mínimo Local",
                          ifelse(f_pp_final < -1e-5, "Máximo Local", "Inflexão"))
  
  tabela_resultados <- rbind(tabela_resultados, data.frame(
    x0 = x0,
    x_Estimado = round(res$raiz, 4),
    f_x = round(res$f_val, 4),
    Iteracoes = res$iter,
    Classificacao = classificacao
  ))
  lista_trajetorias[[as.character(x0)]] <- res$trajetoria
}

print("Resultados do Método de Newton-Raphson:")
print(tabela_resultados)

# -------------------------------------------------------------------------
# 4. Visualização Gráfica
# -------------------------------------------------------------------------
# Plot da função original
curve(f, from = 0, to = 5, lwd = 2, col = "black",
      main = "Convergência de Newton-Raphson em f(x)",
      xlab = "x", ylab = "f(x)")

# Cores para cada valor inicial
cores <- c("red", "blue", "green", "purple")

for (i in 1:length(valores_iniciais)) {
  trajetoria <- lista_trajetorias[[i]]
  # Plotar os pontos da trajetória sobre a curva de f(x)
  points(trajetoria, f(trajetoria), col = cores[i], pch = 20)
  lines(trajetoria, f(trajetoria), col = cores[i], lty = 2)
  
  # Destacar o ponto final
  ponto_final <- tail(trajetoria, 1)
  points(ponto_final, f(ponto_final), col = cores[i], pch = 13, cex = 1.5)
}

legend("top", legend = paste("x0 =", valores_iniciais), 
       col = cores, pch = 20, lty = 2, bty = "n", horiz = TRUE)