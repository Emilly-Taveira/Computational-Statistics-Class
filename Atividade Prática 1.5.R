# ===========================================
# Função Quantil da Distribuição Nakagami
# ===========================================

## --- Implmentação da Densidade --- ##
my.dnakagami   <- function(x, m, omega) {
  aux1              <- 2 * m^m / gamma(m) * omega^m
  aux2              <- x^(2 * m - 1) * exp(- m/omega * x^2)
  dens            <- aux1 * aux2
}

## --- Implementação da Função de Distribuição Acumulada (Aproximação Numérica) --- ##
my.pnakagami     <- function(q, m, omega) {
  sapply(q, function(t) integrate(my.dnakagami, 
                                  lower = 0, 
                                  upper = t, 
                                  m = m, 
                                  omega = omega)$value)
}

result = my.pnakagami(q = seq(0.01, 5, by = 0.01), m = 0.5, omega = 1)
plot(seq(0.01, 5, by = 0.01), result, lwd = 2, col = "red", type = "l", ylim = c(0, 3))

## --- Implementação da Função Quantil (Aproximação Numérica) --- ##

my.qnakagami     <- function(p, m, omega, lower = 0.001, upper = 100)
{
  sapply(p, function(u) {root <- uniroot(function(x) my.pnakagami(x, m, omega) - u, 
                                         interval = c(lower, upper))$root
  return(root)})
}

my.qnakagami(p = 0.5, m = 0.5, omega = 1, lower = 0.1, upper = 1000)

result2 = my.qnakagami(p = seq(0.01, 0.99, by = 0.01), m = 0.5, omega = 1)

plot(seq(0.01, 0.99, by = 0.01), result2, lwd = 2, col = "darkblue", type = "l",
     xlab = "Probability (p)", ylab = "Quantile (q)", main = "Nakagami Quantile Function")
lines(seq(0.01, 0.99, by = 0.01), result2, lwd = 2, col = "darkblue", type = "l")
