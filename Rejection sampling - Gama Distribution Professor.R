## --- Pacotes Necessários --- ##

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(tibble)

## --- Gerar Dados Genéricos --- ##

alpha     <- 3
beta      <- 2
lambda    <- 0.5

x_vals    <- seq(0, 7, length.out = 1000)

df        <- tibble(x = x_vals,
                    Gama = dgamma(x, shape = alpha, rate = beta),
                    Exponencial = dexp(x, rate = lambda),
                    Ratio = dgamma(x, shape = alpha, rate = beta) / dexp(x, rate = lambda))

## --- Preparar Dados p/ o Gráfico de Densidades --- ##

df_long   <- df %>%
  pivot_longer(cols = c(Gama, Exponencial), 
               names_to = "Distribuicao", 
               values_to = "Densidade")

## --- Gráfico das Densidades Gama e Exponencial --- ##

g1        <- ggplot(df_long, aes(x = x,
                                 y = Densidade, 
                                 color = Distribuicao, 
                                 linetype = Distribuicao)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(title = '',
       y = "Densidade") +
  scale_color_manual(values = c("black", "gray50")) +
  theme(legend.position = "left",
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

## --- Gráfico da Razão f(x) / g_lambda(x) --- ##

g2        <- ggplot(df, aes(x = x, y = Ratio)) +
  geom_line(color = "darkblue", linewidth = 1) +
  theme_minimal() +
  labs(title = '',
       y = expression(f(x)/g[lambda](x)), x = "x") +
  theme(axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

## --- Visualização Conjunta dos Gráficos --- ##

g1 + g2

## --- Pacotes Necessários --- ##

library(ggplot2)
library(dplyr)
library(tidyr)

## --- Definir Constante de Normalização --- ##

alpha     <- 3
beta      <- 2
lambda    <- 0.5
x_vals    <- seq(0, 7, length.out = 1000)

c_1       <- (beta^alpha * (alpha - 1)^(alpha - 1) * exp(-(alpha - 1)))
c_2       <- (lambda * gamma(alpha) * (beta - lambda)^(alpha - 1))
c         <-  c_1 / c_2 

## --- Função Envelope e a Densidade Gama --- ##

df2       <- tibble(x = x_vals,
                    Gama = dgamma(x_vals, shape = alpha, rate = beta),
                    Envelope = c * dexp(x_vals, rate = lambda))

## --- Reestruturar Base em Formato Longo --- ##

df2_long  <- df2 %>%
  pivot_longer(cols = c(Gama, Envelope),
               names_to = "Curva",
               values_to = "Densidade")

## --- Gráfico com Curva Gama e Envelope --- ##

ggplot(df2_long, aes(x = x, y = Densidade, color = Curva, linetype = Curva)) +
  geom_line(linewidth = 0.5) +
  theme_minimal() +
  labs(title = '',        
       y = "Densidade") +
  scale_color_manual(values = c("black", "gray50")) +
  theme(legend.position  = "right", 
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

## --- Pacotes Necessários --- ##

library(ggplot2)

## --- Configurações Iniciais --- ##

set.seed(1212)
n         <- 10000
x         <- numeric(n)
i         <- 1
alpha     <- 3
beta      <- 2
lambda    <- 0.5

c_1       <- (beta^alpha * (alpha - 1)^(alpha - 1) * exp(-(alpha - 1)))
c_2       <- (lambda * gamma(alpha) * (beta - lambda)^(alpha - 1))
c         <-  c_1 / c_2

## --- Gerar Amostra Utilizando o Método da Rejeição --- ##

while (i < n)
{
  y       <- rexp(1, rate = lambda)
  u       <- runif(1)
  
  if (u <= dgamma(y, shape = alpha, rate = beta) / (c * dexp(y, rate = lambda))) 
  {
    x[i]  <- y
    i     <- i + 1
  }
}

## --- Histograma Comparado à Densidade Gama --- ##

p1        <- ggplot(data.frame(x = x), aes(x)) +
  geom_histogram(aes(y = ..density.., fill = "Amostra Gerada"), bins = 30,
                 fill = "gray80", color = "black") +
  stat_function(aes(color = "Curva Gama"), 
                fun = function(z) dgamma(z, shape = alpha, rate = beta), 
                linewidth = 0.5) +
  theme_minimal() +
  labs(title = "",
       y = "Densidade") +
  scale_color_manual(name = NULL, values = c("Curva Gama" = "darkred")) +
  theme(legend.position = "none",
        axis.title.x    = element_text(margin = margin(t = 10)),
        axis.title.y    = element_text(margin = margin(r = 10)))

## --- Gráfico e.f.d.a. Comparado à f.d.a. Gama --- ##

p2        <- ggplot(data.frame(x = x), aes(x)) +
  stat_ecdf(aes(color = "e.f.d.a. - Amostra Gerada"), 
            geom = "step", linewidth = 0.8) +
  stat_function(aes(color = "f.d.a. - Gama"), 
                fun = function(z) pgamma(z, shape = alpha, rate = beta), 
                linetype = "dashed", 
                linewidth = 0.5) +
  theme_minimal() +
  labs(title = "",
       y = "Probabilidade Acumulada") +
  scale_color_manual(name = NULL,
                     values = c("e.f.d.a. - Amostra Gerada" = "black",
                                "f.d.a. - Gama" = "darkred")) +
  theme(legend.position = "right",
        axis.title.x    = element_text(margin = margin(t = 10)),
        axis.title.y    = element_text(margin = margin(r = 10)))

## --- Visualização Conjunta dos Gráficos --- ##

p1 + p2