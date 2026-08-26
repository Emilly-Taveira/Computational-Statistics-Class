## --- Pacotes Necessários --- ##
library(ggplot2)

## --- Configurações da Simulação --- ##
set.seed(2026)
n <- 10000              # Número de iterações
mu_x <- 0               # Média da distribuição alvo (Normal padrão)
sigma_x <- 1
mu_imp <- 2             # Média da distribuição de importância (Normal deslocada)

## --- 1. Método de Monte Carlo Simples --- ##
# Alvo: Estimativa de P(X > 3) para X ~ N(0,1)
x_simples <- rnorm(n, mean = mu_x, sd = sigma_x)
estimativa_simples <- mean(x_simples > 3)
erro_simples <- sd(x_simples > 3) / sqrt(n)

## --- 2. Método de Amostragem por Importância --- ##
# Gerando de uma distribuição que favorece a cauda (N(2,1))
x_imp <- rnorm(n, mean = mu_imp, sd = sigma_x)

# Calculando os pesos de verossimilhança (w = f/g)
# w(x) = dnorm(x, 0, 1) / dnorm(x, 2, 1)
pesos <- dnorm(x_imp, mu_x, sigma_x) / dnorm(x_imp, mu_imp, sigma_x)

# Estimativa ponderada
evento <- x_imp > 3
estimativa_imp <- mean(evento * pesos)
erro_imp <- sd(evento * pesos) / sqrt(n)

## --- 3. Resultados Comparativos --- ##
resultados <- data.frame(
  Metodo = c("Monte Carlo Simples", "Amostragem por Importância"),
  Estimativa = c(estimativa_simples, estimativa_imp),
  Erro_Padrao = c(erro_simples, erro_imp)
)

print(resultados)

## --- Visualização --- ##
ggplot(data.frame(x = x_imp), aes(x = x)) +
  geom_density(fill = "gray", alpha = 0.5) +
  geom_vline(xintercept = 3, color = "red", linetype = "dashed") +
  labs(title = "Distribuição da Amostragem por Importância",
       subtitle = "O deslocamento (mean=2) aumenta a frequência de eventos na cauda (X>3)",
       x = "Valor de X", y = "Densidade")