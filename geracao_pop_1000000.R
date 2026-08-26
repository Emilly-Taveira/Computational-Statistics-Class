# =============================================================== 
# 1. Configuração Inicial 
# =============================================================== 

set.seed(241251419) # Usar seu número de matrícula do mestrado/doutorado 
N   <- 1e6 

# =============================================================== 
# 2. Geração das Variáveis Explicativas 
# =============================================================== 

X1  <- abs(rnorm(N, mean = 0.15, sd = 0.04))            # Volatilidade anual (~15%) 
X2  <- rlnorm(N, meanlog = log(18), sdlog = 0.25)       # P/L médio  
X3  <- rnorm(N, mean = 0.003, sd = 0.012)               # Retorno semanal passado 
X4  <- rnorm(N, mean = 0.0007, sd = 0.008)              # Variação cambial semanal 
X5  <- rlnorm(N, meanlog = log(800), sdlog = 0.35)      # Volume médio negociado  
X6  <- rnorm(N, mean = 0.04, sd = 0.004)                # Juros curtos (~4.0% a.a.) 
X7  <- runif(N, min = 5, max = 95)                      # RSI 
X8  <- rnorm(N, mean = 0, sd = 0.4)                     # MACD 
X9  <- rlnorm(N, meanlog = log(1.7), sdlog = 0.15)      # Liquidez corrente 
X10 <- rlnorm(N, meanlog = log(2.5), sdlog = 0.25)      # Alavancagem financeira 

# =============================================================== 
# 3. Construção da Matriz de Preditores com Termos Quadráticos 
# =============================================================== 

dados                 <- data.frame(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10) 
dados_scaled          <- scale(dados, center = TRUE, scale = TRUE) 
dados_quad            <- dados_scaled^2 
colnames(dados_quad)  <- paste0(colnames(dados_scaled), "_2") 
X                     <- cbind(dados_scaled, dados_quad) 
variancias            <- apply(X, 2, var) 
X                     <- X[, variancias > 1e-12]

# =============================================================== 
# 4. Especificação dos Coeficientes e Geração da Resposta 
# =============================================================== 

# Coeficientes dos termos X 

beta <- c(0.25, -0.06, 1.1, -0.28, 0.00012, -0.22, 0.004, 0.09, 0.025, -0.018, 
          -0.45, 0.008, -1.2, 5.8, 0.000002, 1.1, -0.00012, 2.0, -0.012, -0.22) 

if(length(beta) != ncol(X)){ 
  stop("Número de coeficientes não corresponde ao número de preditores após remoção.") 
} 

# Gerar erro aleatório 

erro <- rnorm(N, mean = 0, sd = 4) 

# Definir intercepto 

beta_0 <- 0.6 

# Gerar variável resposta 

Y <- beta_0 + as.vector(as.matrix(X) %*% beta) + erro

# =============================================================== 
# 5. Construção do data.frame Final da População 
# =============================================================== 

populacao <- data.frame(retorno = Y, dados,  
                        dados_quad[, colnames(dados_quad) %in% colnames(X)]) 

# =============================================================== 
# 6. Verificação da População Simulada 
# =============================================================== 
summary(populacao$retorno) 
hist(populacao$retorno, breaks = 100, main = "Distribuição do retorno semanal", 
     xlab = "Retorno (%)") 
head(populacao)