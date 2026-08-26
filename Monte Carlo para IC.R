# ----------- Atividade 3.3 ----------- #
set.seed(123)

amostra = runif(10000, min = 0, max = pi)

funcao = sin(amostra)

estimativa = pi * mean(funcao)

var(amostra)

var = pi^2 * var(amostra) / length(amostra)

# Comparação dos Resultados
resultados    <- data.frame(row.names = c("Estimativa Monte Carlo de θ",
                                          "Estimativa da variância do estimador Monte Carlo de θ",
                                          "Valor exato da integral"),
                            Valor = c(round(estimativa, 6),
                                      round(var, 6),
                                      round(2, 6)))
print(kable(resultados, align = 'c'))


limite_inferior = estimativa - 1.96 * sqrt(var)
limite_superior = estimativa + 1.96 * sqrt(var)

IC_1 <- c(
  LI = limite_inferior,
  Estimativa = estimativa,
  LS = limite_superior
)

# ------- Integral 1: Raiz Quadrada ------- #
library(knitr)

n             <- 10000
X             <- runif(n)

hX            <- sqrt(X)

theta_hat     <- mean(hX)          # Estimador Monte Carlo
sigma2_hat    <- var(hX)           # Variância amostral de h(X)
var_theta_hat <- sigma2_hat / n    # Variância do estimador Monte Carlo

# Comparação dos Resultados
resultados    <- data.frame(row.names = c("Estimativa Monte Carlo de θ",
                                          "Estimativa da variância do estimador Monte Carlo de θ",
                                          "Valor exato da integral"),
                            Valor = c(round(theta_hat, 6),
                                      round(var_theta_hat, 6),
                                      round(0.6666667, 6)))
print(kable(resultados, align = 'c'))

limite_inferior = theta_hat - 1.96 * sqrt(var_theta_hat)
limite_superior = theta_hat + 1.96 * sqrt(var_theta_hat)

IC_2 <- c(
  LI = limite_inferior,
  Estimativa = theta_hat,
  LS = limite_superior
)

# ------- Integral 2: Função Exponencial ------- #
n             <- 10000
X             <- runif(n)

hX            <- exp(X)

theta_hat     <- mean(hX)          # Estimador Monte Carlo
sigma2_hat    <- var(hX)           # Variância amostral de h(X)
var_theta_hat <- sigma2_hat / n    # Variância do estimador Monte Carlo

# Comparação dos Resultados
resultados    <- data.frame(row.names = c("Estimativa Monte Carlo de θ",
                                          "Estimativa da variância do estimador Monte Carlo de θ",
                                          "Valor exato da integral"),
                            Valor = c(round(theta_hat, 6),
                                      round(var_theta_hat, 6),
                                      round(1.7182818, 6)))
print(kable(resultados, align = 'c'))

limite_inferior = theta_hat - 1.96 * sqrt(var_theta_hat)
limite_superior = theta_hat + 1.96 * sqrt(var_theta_hat)

IC_3 <- c(
  LI = limite_inferior,
  Estimativa = theta_hat,
  LS = limite_superior
)

# ------- Integral 3: Função Trigonométrica ------- #
n             <- 10000
X             <- runif(n, min = 0, max = pi/2)

hX            <- cos(X)

theta_hat     <- pi/2 * mean(hX)          # Estimador Monte Carlo
sigma2_hat    <- pi/2 * var(hX)           # Variância amostral de h(X)
var_theta_hat <- sigma2_hat / n    # Variância do estimador Monte Carlo

# Comparação dos Resultados
resultados    <- data.frame(row.names = c("Estimativa Monte Carlo de θ",
                                          "Estimativa da variância do estimador Monte Carlo de θ",
                                          "Valor exato da integral"),
                            Valor = c(round(theta_hat, 6),
                                      round(var_theta_hat, 6),
                                      round(1, 6)))
print(kable(resultados, align = 'c'))

limite_inferior = theta_hat - 1.96 * sqrt(var_theta_hat)
limite_superior = theta_hat + 1.96 * sqrt(var_theta_hat)

IC_3 <- c(
  LI = limite_inferior,
  Estimativa = theta_hat,
  LS = limite_superior
)

# ------- Integral 4: Função Logarítmica ------- #
n             <- 10000
X             <- runif(n, min = 1, max = exp(1))

hX            <- 1/X

theta_hat     <- (exp(1) - 1) * mean(hX)          # Estimador Monte Carlo
sigma2_hat    <-(exp(1) - 1) * var(hX)           # Variância amostral de h(X)
var_theta_hat <- sigma2_hat / n    # Variância do estimador Monte Carlo

# Comparação dos Resultados
resultados    <- data.frame(row.names = c("Estimativa Monte Carlo de θ",
                                          "Estimativa da variância do estimador Monte Carlo de θ",
                                          "Valor exato da integral"),
                            Valor = c(round(theta_hat, 6),
                                      round(var_theta_hat, 6),
                                      round(1, 6)))
print(kable(resultados, align = 'c'))

limite_inferior = theta_hat - 1.96 * sqrt(var_theta_hat)
limite_superior = theta_hat + 1.96 * sqrt(var_theta_hat)

IC_4 <- c(
  LI = limite_inferior,
  Estimativa = theta_hat,
  LS = limite_superior
)

# ------- Integral 5: Integral Dupla (2D) ------- #
n             <- 10000
X             <- runif(n)
Y             <- runif(n)

hX            <- X^2 + Y^2

theta_hat     <- mean(hX)          # Estimador Monte Carlo
sigma2_hat    <- var(hX)           # Variância amostral de h(X)
var_theta_hat <- sigma2_hat / n    # Variância do estimador Monte Carlo

# Comparação dos Resultados
resultados    <- data.frame(row.names = c("Estimativa Monte Carlo de θ",
                                          "Estimativa da variância do estimador Monte Carlo de θ",
                                          "Valor exato da integral"),
                            Valor = c(round(theta_hat, 6),
                                      round(var_theta_hat, 6),
                                      round(0.6666667, 6)))
print(kable(resultados, align = 'c'))

limite_inferior = theta_hat - 1.96 * sqrt(var_theta_hat)
limite_superior = theta_hat + 1.96 * sqrt(var_theta_hat)

IC_5 <- c(
  LI = limite_inferior,
  Estimativa = theta_hat,
  LS = limite_superior
)

# ------- Integral 6: Área de um Círculo ------- #
n             <- 10000
X             <- runif(n, min = -1, max = 1)
Y             <- runif(n, min = -1, max = 1)

hX            <- (X^2 + Y^2 <= 1)

theta_hat     <- 4 * mean(hX)          # Estimador Monte Carlo
sigma2_hat    <- 4 * var(hX)       # Variância amostral de h(X)
var_theta_hat <- sigma2_hat / n    # Variância do estimador Monte Carlo

# Comparação dos Resultados
resultados    <- data.frame(row.names = c("Estimativa Monte Carlo de θ",
                                          "Estimativa da variância do estimador Monte Carlo de θ",
                                          "Valor exato da integral"),
                            Valor = c(round(theta_hat, 6),
                                      round(var_theta_hat, 6),
                                      round(3.1415926536, 6)))
print(kable(resultados, align = 'c'))

limite_inferior = theta_hat - 1.96 * sqrt(var_theta_hat)
limite_superior = theta_hat + 1.96 * sqrt(var_theta_hat)

IC_6 <- c(
  LI = limite_inferior,
  Estimativa = theta_hat,
  LS = limite_superior
)

tab = data.frame(IC_1, IC_2, IC_3, IC_4, IC_5, IC_6)
print(kable(t(tab)))

      