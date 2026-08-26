# ==============================================================================
# Simulation of Negative Binomial & Chi-Square Goodness-of-Fit Test
# ==============================================================================

set.seed(456) # For reproducibility

# Parameters Setup
n <- 500
r_param <- 5
p_param <- 0.4

sample_nb <- rnbinom(n, size = r_param, prob = p_param)

obs_counts <- table(sample_nb)
val_names <- as.numeric(names(obs_counts))
max_val <- max(val_names)

probs <- dnbinom(0:max_val, size = r_param, prob = p_param)
expected <- n * probs

break_point <- max(which(expected >= 5)) - 1

obs_grouped <- c(obs_counts[1:break_point], sum(obs_counts[(break_point + 1):length(obs_counts)]))
prob_grouped <- c(probs[1:break_point], 1 - sum(probs[1:break_point]))
exp_grouped <- n * prob_grouped

# 4. Perform Chi-Square Test
chisq_res <- chisq.test(obs_grouped, p = prob_grouped)

cat("--- Chi-Square Test Results ---\n")
cat("Statistic X^2:", chisq_res$statistic, "\n")
cat("Degrees of Freedom:", chisq_res$parameter, "\n")
cat("P-value:", chisq_res$p.value, "\n")
cat("Conclusion:", ifelse(chisq_res$p.value > 0.05, 
                          "Fail to reject H0 (Consistent with NegBinomial)", 
                          "Reject H0"), "\n\n")

# Visualizations
par(mfrow = c(1, 1))
barplot_data <- rbind(obs_grouped, exp_grouped)
colnames(barplot_data) <- c(0:(break_point-1), paste0(break_point, "+"))

barplot(barplot_data, beside = TRUE, col = c("royalblue", "orange"),
        main = "Observed vs Expected Frequencies",
        xlab = "Number of Failures (k)", ylab = "Frequency",
        legend.text = c("Observed", "Expected"))

# Replications (B = 100)
B <- 100
p_vals_rep <- numeric(B)

for (i in 1:B) {
  rep_sample <- rnbinom(n, size = r_param, prob = p_param)
  rep_obs <- table(rep_sample)
  obs_rep_grouped <- c(rep_obs[as.numeric(names(rep_obs)) < break_point], 
                       sum(rep_obs[as.numeric(names(rep_obs)) >= break_point]))
  
  final_obs <- numeric(length(prob_grouped))
  names(final_obs) <- 1:length(prob_grouped)
  p_vals_rep[i] <- chisq.test(obs_grouped, p = prob_grouped)$p.value
}

acceptance_rate <- mean(p_vals_rep > 0.05)
cat("--- Replication Results ---\n")
cat("Acceptance Rate:", acceptance_rate * 100, "%\n")