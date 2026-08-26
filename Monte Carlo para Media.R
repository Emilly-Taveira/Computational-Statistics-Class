# ------- Integral 1: Função Exponencial Simples ------- #
x = runif(10000)
theta_hat1 = mean(exp(x))

# Gráfico
n = 10000
y = exp(x)
theta_acum = cumsum(y) / seq_along(y)
theta_exact = 1.7182818

plot(x = 1:n, y = theta_acum, type = "l", lwd = 2)
abline(h = theta_exact, col = "red", lwd = 2)


# ------- Integral 2: Função Polinomial ------- #
func = function(x) x^3 - 2 * x^2 + 3 * x - 1
x = runif(10000, min = 0, max = 2)
theta_hat2 = 2 * mean(func(x))

# Gráfico
n = 10000
y = func(x)
theta_acum = 2 * cumsum(y) / seq_along(y)
theta_exact = 2.666666667

plot(x = 1:n, y = theta_acum, type = "l", lwd = 2)
abline(h = theta_exact, col = "red", lwd = 2)

# ------- Integral 3: Função Trigonométrica ------- #
x = runif(10000, min = 0, max = pi/2)
theta_hat3 = pi/2 * mean(sin(x))

# Gráfico
n = 10000
y = sin(x)
theta_acum = pi/2 * cumsum(y) / seq_along(y)
theta_exact = 1

plot(x = 1:n, y = theta_acum, type = "l", lwd = 2)
abline(h = theta_exact, col = "red", lwd = 2)

# -------  Integral 4: Função com Singularidade Evitável ------- #
x = runif(10000)
theta_hat4 = mean(sin(x)/x)

# Gráfico
n = 10000
y = sin(x)/x
theta_acum = cumsum(y) / seq_along(y)
theta_exact = 0.9460830704

plot(x = 1:n, y = theta_acum, type = "l", lwd = 2)
abline(h = theta_exact, col = "red", lwd = 2)

# -------  Integral 5: Integral Dupla (2D) ------- #
x = runif(10000)
y = runif(10000)
theta_hat5 = mean( exp(-(x+y)) )

# Gráfico
n = 10000
y = exp(-(x+y))
theta_acum = cumsum(y) / seq_along(y)
theta_exact = 0.3995764

plot(x = 1:n, y = theta_acum, type = "l", lwd = 2)
abline(h = theta_exact, col = "red", lwd = 2)

# -------  Integral 6: Área de um Círculo ------- #
n = 10000
x = runif(10000, min = -1, max = 1)
y = runif(10000, min = -1, max = 1)
amostra_valida = ifelse(x^2 + y^2 <= 1, TRUE, FALSE)
m = sum(amostra_valida == TRUE)
theta_hat6 = 4 * m / n

# ------- Integral 7: Função com Cauda (Domínio Infinito) ------- #
func = function(U) -log(U)
U = runif(10000)
x = func(U)
theta_hat7 = mean( exp(-x) / exp(-x) )

# ------- Integral 8: Função Oscilatória ------- #
x = runif(10000, min = 0, max = 2 * pi)
theta_hat8 = 2 * pi * mean(sin(x)^2)

# ------- Integral 9: Integral em Domínio Não Retangular ------- #
x = runif(10000)
y = runif(10000)
indicadora = ifelse(y <= x, 1, 0)
theta_hat9 = mean( exp(y) * indicadora ) 

# Tabela com os resultados
result = data.frame(row.names = paste0("Estimador de θ", 1:9),
                    Values = c(theta_hat1,
                                theta_hat2,
                                theta_hat3,
                                theta_hat4,
                                theta_hat5,
                                theta_hat6,
                                theta_hat7,
                                theta_hat8,
                                theta_hat9))

kable(result)


####################################################################

# ------- Integral 1: Raiz Quadrada ------- #
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
kable(resultados, align = 'c')

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
kable(resultados, align = 'c')

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
kable(resultados, align = 'c')

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
kable(resultados, align = 'c')

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
kable(resultados, align = 'c')

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
kable(resultados, align = 'c')
