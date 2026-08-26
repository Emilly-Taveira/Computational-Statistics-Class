# Amostra da exponencial

sample = rexp(n = 50, rate = 2)

## --- Valores Iniciais --- ##

n = length(sample)
sample_mean = mean(sample)
analytical_MLE = 1/ sample_mean
max.iter = 100 
tol = 1e-6

## --- Derivada da Função Objetivo --- ##

df   <- function(n, lamb, x) 
{
  n / lamb - n * mean(x)
}

## --- Implementação do Método da Bissecção --- ##

bissecao_emv <- function(func, c(a, b), tol, max.iter){
  
  if( func(a) * func(b) < 0 ){
    i = 1
    while (i <= max.iter) {
      x <- a + (b - a)/2
      
      
    }
  }

}



## --- Resultados --- ##

list('Ponto Ótimo' = x,
     'Função Objetivo no Ponto Ótimo' = dexp(x),
     'Derivada no Ponto Ótimo' = round(df(x), 0)
)