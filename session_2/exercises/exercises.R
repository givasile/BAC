# 1. Estimate expected value of a Gamma(alpha, beta) distribution
alpha <- 2
beta <- 3  # Rate parameter
set.seed(123)
samples_gamma <- rgamma(10000, shape = alpha, rate = beta)
expected_gamma <- mean(samples_gamma)
cat("Estimated expected value of Gamma(", alpha, ",", beta, ") = ", expected_gamma, "\n")

# 2. Estimated variance of the approximation
variance_gamma <- var(samples_gamma)
cat("Estimated variance of the approximation for Gamma distribution = ", variance_gamma, "\n")

# 3. Standard error of the approximation
standard_error_gamma <- sqrt(variance_gamma / length(samples_gamma))
cat("Estimation is: ", expected_gamma, "with +- ", standard_error_gamma, "\n")

# 4. Estimate expected value of a Beta(alpha, beta) distribution
alpha_beta <- 2
beta_beta <- 5
set.seed(123)
samples_beta <- rbeta(10000, shape1 = alpha_beta, shape2 = beta_beta)
expected_beta <- mean(samples_beta)
cat("Estimated expected value of Beta(", alpha_beta, ",", beta_beta, ") = ", expected_beta, "\n")

# 5. Estimated variance of the approximation
variance_beta <- var(samples_beta)
cat("Estimated variance of the approximation for Beta distribution = ", variance_beta, "\n")

# 6. Standard error of the approximation
standard_error_beta <- sqrt(variance_beta / length(samples_beta))
cat("Estimation is: ", expected_beta, "with +- ", standard_error_beta, "\n")




##### Exercise Central Limit theorem

set.seed(123)

library(ggplot2)
library(gridExtra)

n_samples <- 10000
n_sum <- 30  # number of samples to sum for CLT effect

# 1. Uniform distribution (non-Gaussian)
uniform_samples <- runif(n_samples, min = 0, max = 1)

# 2. Exponential distribution (right-skewed)
exp_samples <- rexp(n_samples, rate = 1)

# 3. Bernoulli distribution (binary, discrete)
bern_samples <- rbinom(n_samples, size = 1, prob = 0.3)

# Plot the original distributions
p1 <- ggplot(data.frame(x = uniform_samples), aes(x)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  ggtitle("Uniform(0,1)")

p2 <- ggplot(data.frame(x = exp_samples), aes(x)) +
  geom_histogram(bins = 30, fill = "salmon", color = "black") +
  ggtitle("Exponential(1)")

p3 <- ggplot(data.frame(x = bern_samples), aes(x = factor(x))) +
  geom_bar(fill = "lightgreen", color = "black") +
  ggtitle("Bernoulli(0.3)") + xlab("Value") + ylab("Count")

# Function to compute sums of n_sum samples
clt_samples <- function(x) {
  replicate(n_samples, mean(sample(x, n_sum, replace = TRUE)))
}

# Generate CLT samples
uniform_sum <- clt_samples(uniform_samples)
exp_sum <- clt_samples(exp_samples)
bern_sum <- clt_samples(bern_samples)

# Plot the sums
p4 <- ggplot(data.frame(x = uniform_sum), aes(x)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  ggtitle(paste("Sum of", n_sum, "Uniform samples"))

p5 <- ggplot(data.frame(x = exp_sum), aes(x)) +
  geom_histogram(bins = 30, fill = "salmon", color = "black") +
  ggtitle(paste("Sum of", n_sum, "Exponential samples"))

p6 <- ggplot(data.frame(x = bern_sum), aes(x)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  ggtitle(paste("Sum of", n_sum, "Bernoulli samples"))

# Show all plots
grid.arrange(p1, p4, p2, p5, p3, p6, ncol = 2)

# Save images 
ggsave(filename = "figures/uniform.png", plot = p1, width = 6, height = 4, dpi = 150)
ggsave(filename = "figures/exponential.png", plot = p2, width = 6, height = 4, dpi = 150)
ggsave(filename = "figures/bernoulli.png", plot = p3, width = 6, height = 4, dpi = 150)
png("figures/clt_demo.png", width = 1200, height = 1600, res = 150)
grid.arrange(p1, p4, p2, p5, p3, p6, ncol = 2)
dev.off()