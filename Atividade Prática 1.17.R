# ==============================================================================
# Simulation of Log-Normal Distribution & Goodness-of-Fit Testing
# ==============================================================================

# Setup and Libraries
if (!require(goftest)) install.packages("goftest")
library(goftest)

set.seed(123) # For reproducibility
n <- 1000
mu_val <- 1.0
sigma_val <- 0.5

rlnorm_custom <- function(n, mu, sigma) {
  u <- runif(n)
  samples <- exp(mu + sigma * qnorm(u))
  return(samples)
}

sample_data <- rlnorm_custom(n, mu_val, sigma_val)


cvm_result <- cvm.test(sample_data, "plnorm", meanlog = mu_val, sdlog = sigma_val)

cat("--- Single Simulation Results ---\n")
cat("Statistic W^2:", cvm_result$statistic, "\n")
cat("P-value:", cvm_result$p.value, "\n")
cat("Conclusion:", ifelse(cvm_result$p.value > 0.05, 
                          "Fail to reject H0 (Data follows Log-Normal)", 
                          "Reject H0"), "\n\n")

# Visualizations
par(mfrow = c(1, 2))

# Plot A: Histogram vs Theoretical Density
hist(sample_data, breaks = 30, probability = TRUE, 
     main = "Density Comparison", xlab = "x", col = "gray90")
curve(dlnorm(x, meanlog = mu_val, sdlog = sigma_val), 
      add = TRUE, col = "firebrick", lwd = 2)
legend("topright", legend = "Theoretical Log-Normal", col = "firebrick", lwd = 2, cex = 0.8)

# Empirical vs Theoretical CDF
plot(ecdf(sample_data), main = "CDF Comparison", xlab = "x", ylab = "F(x)")
curve(plnorm(x, meanlog = mu_val, sdlog = sigma_val), 
      add = TRUE, col = "royalblue", lwd = 2)
legend("bottomright", legend = c("Empirical", "Theoretical"), 
       col = c("black", "royalblue"), lwd = 1:2, cex = 0.8)

B <- 100
p_values_true <- numeric(B)
p_values_false <- numeric(B)

cat("Running replications (B = 100)...\n")

for (i in 1:B) {
  # Case A: True Null Hypothesis (The data IS Log-Normal)
  s_true <- rlnorm_custom(n, mu_val, sigma_val)
  p_values_true[i] <- cvm.test(s_true, "plnorm", meanlog = mu_val, sdlog = sigma_val)$p.value
  
  # Case B: False Null Hypothesis (The data is actually Weibull)
  s_false <- rweibull(n, shape = 2, scale = 3) 
  p_values_false[i] <- cvm.test(s_false, "plnorm", meanlog = mu_val, sdlog = sigma_val)$p.value
}

# Summary Statistics
acceptance_rate <- mean(p_values_true > 0.05)
rejection_rate_false <- mean(p_values_false < 0.05)

cat("--- Power Analysis Results ---\n")
cat("Acceptance Rate (True H0):", acceptance_rate * 100, "%\n")
cat("Power of the Test (Rejecting False H0):", rejection_rate_false * 100, "%\n")