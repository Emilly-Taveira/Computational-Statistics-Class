## --- Pacotes Necessários --- ##

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(tibble)

## --- Gerar Dados Genéricos --- ##

x_vals    <- seq(-4, 4, length.out = 1000)

df        <- tibble(x       = x_vals,
                    Normal  = dnorm(x),
                    Cauchy  = dcauchy(x),
                    Ratio   = dnorm(x) / dcauchy(x))

## --- Preparar Dados p/ o Gráfico de Densidades --- ##

df_long   <- df %>%
  pivot_longer(cols       = c(Normal, Cauchy),
               names_to   = "Distribuição",
               values_to  = "Densidade")

## --- Gráfico das Densidades Normal e Cauchy --- ##

g1        <- ggplot(df_long, aes(x = x, y = Densidade, color = Distribuição, linetype = Distribuição)) +
  geom_line(linewidth = 0.5) +
  theme_minimal() +
  labs(title = '',
       y     = "Densidade") +
  scale_color_manual(values = c("black", "gray50")) +
  theme(legend.position = "left",
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

## --- Gráfico da Razão f(x) / g_theta(x) --- ##

g2        <- ggplot(df, aes(x = x, y = Ratio)) +
  geom_line(color = "darkblue", linewidth = 0.5) +
  geom_vline(xintercept = c(-1, 1), linetype = "dashed") +
  theme_minimal() +
  labs(title = '',
       y     = expression(f(x) / g[theta](x)),
       x     = "x") +
  theme(axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

## --- Visualização Conjunta dos Gráficos --- ##

g1 + g2

## --- Pacotes Necessários --- ##

library(ggplot2)
library(dplyr)
library(tidyr)

## --- Definir Constante de Normalização --- ##

c         <- sqrt(2 * pi / exp(1))

## --- Calcular a Função Envelope e a Densidade Normal --- ##

df2       <- tibble(x         = x_vals,
                    Normal    = dnorm(x_vals),
                    Envelope  = c * dcauchy(x_vals))

## --- Reestruturar Base em Formato Longo --- ##

df2_long  <- df2 %>%
  pivot_longer(cols      = c(Normal, Envelope), 
               names_to  = "Curva", 
               values_to = "Densidade")

## --- Gráfico com Curva Normal e Envelope --- ##

ggplot(df2_long, aes(x         = x, 
                     y         = Densidade, 
                     color     = Curva, 
                     linetype  = Curva)) +
  geom_line(linewidth     = 1) +
  theme_minimal() +
  labs(title              = '',
       y                  = "Densidade") +
  scale_color_manual(values = c("black", "gray50")) +
  theme(legend.position  = "right", 
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))


## --- Pacotes Necessários --- ##

library(ggplot2)

## --- Configurações Iniciais --- ##

set.seed(1212)
n         <- 10000
c         <- sqrt(2 * pi / exp(1))
x         <- numeric(n)
i         <- 1

## --- Gerar Amostra Utilizando o Método da Rejeição --- ##

while (i < n)
{
  y       <- rcauchy(1)
  u       <- runif(1)
  
  if (u <= dnorm(y) / (c * dcauchy(y))) 
  {
    x[i]  <- y
    i     <- i + 1
  }
}

## --- Construir Histograma Comparado à Densidade Normal Padrão --- ##

p1        <- ggplot(data.frame(x = x), aes(x)) +
  geom_histogram(aes(y = ..density.., fill = "Amostra Gerada"), 
                 bins = 30,
                 fill = "gray80", color = "black") +
  stat_function(aes(color = "Curva Normal"), 
                fun = dnorm, 
                linewidth = 1) +
  theme_minimal() +
  labs(title = "",
       y     = "Densidade") +
  scale_color_manual(name = NULL, 
                     values = c("Curva Normal" = "darkred")) +
  theme(legend.position = "none",
        axis.title.x    = element_text(margin = margin(t = 10)),
        axis.title.y    = element_text(margin = margin(r = 10)))

## --- Construir Gráfico e.f.d.a. Comparado à f.d.a. Normal Padrão --- ##

p2        <- ggplot(data.frame(x = x), aes(x)) +
  stat_ecdf(aes(color = "e.f.d.a. - Amostra Gerada"), 
            geom = "step", 
            linewidth = 0.8) +
  stat_function(aes(color = "f.d.a. - Normal Padrão"), 
                fun = pnorm, 
                linetype = "dashed", 
                linewidth = 0.8) +
  theme_minimal() +
  labs(title = "",
       y     = "Probabilidade Acumulada") +
  scale_color_manual(name = NULL,
                     values = c("e.f.d.a. - Amostra Gerada" = "black",
                                "f.d.a. - Normal Padrão" = "darkred")) +
  theme(legend.position  = "right", 
        axis.title.x     = element_text(margin = margin(t = 10)),
        axis.title.y     = element_text(margin = margin(r = 10)))

## --- Visualização Conjunta dos Gráficos --- ##

p1 + p2
p2
