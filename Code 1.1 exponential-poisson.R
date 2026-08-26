###### Code 1.1 ###### 
# Implementation of the density function of the exponential-poisson distribuition

my.dexp <- function(x, lamb, bet) {
  
  # It is recommended to break the whole function into smaller parts for better efficiency
  part1 = lamb * bet / (1 - exp(-lamb))
  part2 = exp(-lamb - bet * x + lamb * exp(-bet * x))
  
  density = part1 * part2
  
  return(density)
}