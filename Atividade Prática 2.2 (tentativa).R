# Gráfico da função para inspeção e encontrar o valor inicial

# Objetive Function
func <-  function(x) x^4 - 8 * x^3 + 18 * x^2 - 11 * x + 5

x_values = seq(0, 5, by = 0.1)
y_values = sapply(x_values, func)

plot(x_values, y_values, type = "l")

# Exercise Data

first_derivative = function(x) 4 * x^3 - 24 * x^2 + 36 * x - 11
second_derivative = function(x) 12 * x^2 - 48 * x + 36
initial_values = c(0, 1.5, 3, 4.5)
tolerance = 1e-8
max_interations = 100

# Newton-Raphson Method

iter = 10
x = 0.5

for(i in 1:iter) {
  cat(sprintf("Iteração %d:\n", i))
  cat(sprintf("  x^(k)       = %.8f\n", x))
  cat(sprintf("  f'(x^(k))   = %.8f\n", first_derivative(x)))
  cat(sprintf("  f''(x^(k))  = %.8f\n", second_derivative(x)))
  
  x = x - first_derivative(x) / second_derivative(x)
  
  cat(sprintf("  Atualizado para x^(k+1) = %.8f\n\n", x))
}


newton_raphson <- function(f1, f2, init, tol, max.iter){
  
}

