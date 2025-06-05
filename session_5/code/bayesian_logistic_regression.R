library(ggplot2)

set.seed(42)

# Number of samples
N <- 50

# Input features x1, x2 ~ Uniform(-3, 3)
X <- matrix(runif(2 * N, min = -3, max = 3), ncol = 2)

# Ground truth weights (no bias)
w_true <- c(1., -1.)

# Linear predictor
logits <- X %*% w_true

# Sigmoid to get probabilities
sigmoid <- function(z) 1 / (1 + exp(-z))
probs <- sigmoid(logits)

# Binary labels y ~ Bernoulli(prob)
y <- rbinom(N, size = 1, prob = probs)

# Convert to data frame for ggplot
df <- data.frame(x1 = X[, 1], x2 = X[, 2], y = as.factor(y))

# Create the plot
p <- ggplot(df, aes(x = x1, y = x2, color = y)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("red", "blue"), labels = c("y = 0", "y = 1")) +
  labs(title = "Generated Dataset", x = "x1", y = "x2", color = "Label") +
  theme_minimal()

p <- ggplot(df, aes(x = x1, y = x2, color = y)) +
  geom_point(size = 4) +                  # Bigger dots (default is ~2)
  scale_color_manual(values = c("red", "blue"), labels = c("y = 0", "y = 1")) +
  labs(title = "Generated Dataset", x = "x1", y = "x2", color = "Label") +
  theme_minimal(base_size = 18) +        # Bigger base font size for axis, labels, legend
  theme(
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18)
  )


print(p)
# Save to PDF
ggsave("figures/generated_dataset.pdf", plot = p)


# Laplace approxiamtion

library(numDeriv)  # for Hessian

# Data: X (N x 2), y (N x 1), prior precision alpha (scalar)
fit_bayesian_logistic_laplace <- function(X, y, alpha = 1.0) {
  
  N <- nrow(X)
  d <- ncol(X)
  
  # Sigmoid function
  sigmoid <- function(z) 1 / (1 + exp(-z))
  
  # Log prior (Gaussian zero-mean)
  log_prior <- function(w) {
    -0.5 * alpha * sum(w^2)
  }
  
  # Log likelihood for logistic regression
  log_likelihood <- function(w) {
    eta <- X %*% w
    sum(y * eta - log(1 + exp(eta)))
  }
  
  # Negative log posterior (to minimize)
  neg_log_post <- function(w) {
    - (log_prior(w) + log_likelihood(w))
  }
  
  # Optimize to find MAP estimate
  opt <- optim(
    par = rep(0, d),  # init at zero
    fn = neg_log_post,
    method = "BFGS",
    hessian = TRUE
  )
  
  w_map <- opt$par
  
  # Compute Hessian of negative log posterior at MAP
  hess <- hessian(neg_log_post, w_map)
  
  # Covariance of Gaussian approx = inverse Hessian
  Sigma <- solve(hess)
  
  list(mean = w_map, cov = Sigma)
}

# Example usage with your data X, y and alpha=1:
result <- fit_bayesian_logistic_laplace(X, y, alpha=1)
print(result$mean)  # MAP estimate
print(result$cov)   # Covariance matrix of approximate posterior

library(MASS)     # for mvrnorm sampling
library(ggplot2)
library(gridExtra) # for arranging plots

# 1) Visualize Gaussian posterior on (w1,w2) parameter space
plot_laplace_posterior <- function(mean, cov) {
  w1 <- seq(mean[1] - 3*sqrt(cov[1,1]), mean[1] + 3*sqrt(cov[1,1]), length.out=100)
  w2 <- seq(mean[2] - 3*sqrt(cov[2,2]), mean[2] + 3*sqrt(cov[2,2]), length.out=100)
  grid <- expand.grid(w1 = w1, w2 = w2)
  
  # Multivariate normal density function
  dmvn <- function(x, mu, Sigma) {
    k <- length(mu)
    det_sigma <- det(Sigma)
    inv_sigma <- solve(Sigma)
    diff <- matrix(x - mu, ncol = 1)
    exp(-0.5 * t(diff) %*% inv_sigma %*% diff) / sqrt((2*pi)^k * det_sigma)
  }
  
  # Compute densities over grid
  grid$z <- apply(grid, 1, function(row) dmvn(as.numeric(row), mean, cov))
  
  ggplot(grid, aes(w1, w2, z = z)) +
    geom_contour_filled(alpha=0.7) +
    theme(legend.position = "none") +     # <- remove entire legend +
    guides(fill = "none") +
    labs(title = "Laplace Approximation: Posterior over (w1, w2)",
         x = expression(w[1]), y = expression(w[2])) +
    theme_minimal(base_size = 16)
}

# 2) Sample decision boundaries from posterior and plot over data points
plot_decision_boundaries <- function(X, y, mean, cov, n_samples = 15) {
  # Sample weights from Gaussian approx
  samples <- mvrnorm(n_samples, mu = mean, Sigma = cov)
  
  df_data <- data.frame(x1 = X[,1], x2 = X[,2], y = factor(y))
  
  # Base plot with data points
  base_plot <- ggplot(df_data, aes(x = x1, y = x2, color = y)) +
    geom_point(size = 3) +
    scale_color_manual(values = c("red", "blue")) +
    labs(title = "Decision Boundaries Sampled from Posterior",
         color = "Class") +
    theme_minimal(base_size = 16)
  
  # For each sample, add a decision boundary line
  for (i in 1:n_samples) {
    w <- samples[i,]
    # The decision boundary is where w1 * x1 + w2 * x2 = 0
    # => x2 = - (w1/w2) * x1
    slope <- -w[1]/w[2]
    intercept <- 0
    
    base_plot <- base_plot +
      geom_abline(intercept = intercept, slope = slope,
                  color = "black", alpha = 0.2)
  }
  
  base_plot
}

# --- Example usage ---
# Assume X, y are your data and result is Laplace approx output

# Save figures
result <- fit_bayesian_logistic_laplace(X, y, alpha=1)

p1 <- plot_laplace_posterior(result$mean, result$cov)
print(p1)
ggsave("figures/laplace_posterior_contours.pdf", p1, width = 6, height = 6)

p2 <- plot_decision_boundaries(X, y, result$mean, result$cov)
print(p2)
ggsave("figures/laplace_decision_boundaries.pdf", p2, width = 6, height = 6)

# Arrange side-by-side



##########  Part 2
# Creating R code that defines two importance sampling functions:
# (1) using the prior as proposal
# (2) using the Laplace approximation as proposal
# Then visualizing 30 decision boundaries for each.

# Load necessary libraries
library(ggplot2)
library(MASS)  # for mvrnorm
library(dplyr)

# Sigmoid function
sigmoid <- function(z) 1 / (1 + exp(-z))

# Log posterior up to a constant
log_posterior <- function(theta, X, y, alpha = 1) {
  z <- X %*% theta
  log_lik <- sum(y * log(sigmoid(z)) + (1 - y) * log(1 - sigmoid(z)))
  log_prior <- -0.5 * alpha * sum(theta^2)
  log_lik + log_prior
}

importance_sampling_prior <- function(X, y, n_samples = 100, n_keep = 30, alpha = 1.0) {
  d <- ncol(X)
  
  # Sample from prior: theta ~ N(0, alpha^-1 * I)
  prior_sd <- 1 / sqrt(alpha)
  all_samples <- matrix(rnorm(n_samples * d, mean = 0, sd = prior_sd), nrow = n_samples)
  
  # Compute unnormalized log weights
  log_weights <- apply(all_samples, 1, function(theta) log_posterior(theta, X, y, alpha))
  
  # Normalize weights
  log_weights <- log_weights - max(log_weights)  # for stability
  weights <- exp(log_weights)
  weights <- weights / sum(weights)
  
  # Resample 30 samples according to the importance weights
  selected_indices <- sample(1:n_samples, size = n_keep, replace = FALSE, prob = weights)
  selected_samples <- all_samples[selected_indices, ]
  
  return(list(samples = selected_samples, weights = weights[selected_indices]))
}

# Assuming X and y are already defined as in your example
set.seed(123)  # for reproducibility

res_prior <- importance_sampling_prior(X, y, n_samples = 300, n_keep = 30, alpha = 1.0)

# Access the samples and weights
samples_prior <- res_prior$samples
weights_prior <- res_prior$weights

# View the first few samples and weights
head(samples_prior)
head(weights_prior)

library(ggplot2)

plot_decision_boundaries_2 <- function(samples, X, y, n_samples = 15) {
  # Use at most n_samples from samples
  n_samples <- min(n_samples, nrow(samples))
  samples <- samples[1:n_samples, , drop = FALSE]
  
  df_data <- data.frame(x1 = X[,1], x2 = X[,2], y = factor(y))
  
  # Base plot with data points
  base_plot <- ggplot(df_data, aes(x = x1, y = x2, color = y)) +
    geom_point(size = 3) +
    scale_color_manual(values = c("red", "blue")) +
    labs(title = "Decision Boundaries Sampled from Posterior",
         color = "Class") +
    theme_minimal(base_size = 16)
  
  # Add decision boundaries for each sample
  for (i in 1:n_samples) {
    w <- samples[i,]
    
    if (abs(w[2]) < 1e-6) {
      # Vertical line if w2 ~ 0
      intercept <- 0
      base_plot <- base_plot +
        geom_vline(xintercept = intercept, color = "black", alpha = 0.3, size = 0.5)
    } else {
      slope <- -w[1]/w[2]
      intercept <- 0
      base_plot <- base_plot +
        geom_abline(intercept = intercept, slope = slope,
                    color = "black", alpha = 0.3, size = 0.5)
    }
  }
  
  base_plot
}

p <- plot_decision_boundaries_2(samples_prior, X, y, n_samples = 30)

print(p)
ggsave("figures/importance_sampling_from_prior.pdf", p, width = 6, height = 6)

importance_sampling_laplace <- function(X, y, mean, cov, n_samples = 300, n_keep = 30, alpha = 1.0) {
  d <- ncol(X)
  
  # Sample from Laplace approx: theta ~ N(mean, cov)
  all_samples <- MASS::mvrnorm(n_samples, mu = mean, Sigma = cov)
  
  # Compute log target posterior for each sample
  log_post <- apply(all_samples, 1, function(theta) log_posterior(theta, X, y, alpha))
  
  # Compute log proposal density for each sample (Laplace approx)
  log_proposal <- mvtnorm::dmvnorm(all_samples, mean = mean, sigma = cov, log = TRUE)
  
  # Importance weights = target / proposal (in log scale)
  log_weights <- log_post - log_proposal
  
  # Normalize weights for numerical stability
  log_weights <- log_weights - max(log_weights)
  weights <- exp(log_weights)
  weights <- weights / sum(weights)
  
  # Resample n_keep samples with weighted sampling (without replacement)
  selected_indices <- sample(1:n_samples, size = n_keep, replace = FALSE, prob = weights)
  selected_samples <- all_samples[selected_indices, ]
  
  return(list(samples = selected_samples, weights = weights[selected_indices]))
}


set.seed(123)  # for reproducibility

res_laplace <- importance_sampling_laplace(X, y, result$mean, result$cov, n_samples = 100, n_keep = 30, alpha = 1.0)

samples_laplace <- res_laplace$samples
weights_laplace <- res_laplace$weights

# View a few samples and weights
head(samples_laplace)
head(weights_laplace)


p <- plot_decision_boundaries_2(samples_laplace, X, y, n_samples = 30)
print(p)

ggsave("figures/importance_sampling_from_laplace.pdf", p, width = 6, height = 6)



#################### Part 3


library(MASS)  # for mvrnorm

# Logistic sigmoid
sigmoid <- function(z) 1 / (1 + exp(-z))

# Log-posterior (up to constant)
log_posterior <- function(theta, X, y, alpha = 1.0) {
  eta <- X %*% theta
  log_lik <- sum(y * eta - log(1 + exp(eta)))
  log_prior <- -0.5 * alpha * sum(theta^2)
  return(log_lik + log_prior)
}

# MCMC sampler: Metropolis-Hastings
mcmc_logistic <- function(X, y, n_samples = 1000, burn_in = 200, thin = 1,
                          init = NULL, step_size = 0.1, alpha = 1.0) {
  d <- ncol(X)
  if (is.null(init)) {
    current <- rep(0, d)
  } else {
    current <- init
  }
  samples <- matrix(NA, nrow = n_samples, ncol = d)
  current_log_post <- log_posterior(current, X, y, alpha)
  
  for (i in 1:(n_samples * thin + burn_in)) {
    # Propose new sample from Gaussian random walk
    proposal <- current + rnorm(d, mean = 0, sd = step_size)
    proposal_log_post <- log_posterior(proposal, X, y, alpha)
    
    # Acceptance probability
    log_accept_ratio <- proposal_log_post - current_log_post
    if (log(runif(1)) < log_accept_ratio) {
      current <- proposal
      current_log_post <- proposal_log_post
    }
    
    # Save samples after burn-in and thinning
    if (i > burn_in && (i - burn_in) %% thin == 0) {
      samples[(i - burn_in) / thin, ] <- current
    }
  }
  
  return(samples)
}

# Assume X, y already defined
set.seed(42)
samples_mcmc <- mcmc_logistic(X, y, n_samples = 5000, burn_in = 1000, thin = 10)

p <- plot_decision_boundaries_2(samples_mcmc, X, y, n_samples = 30)
print(p)
ggsave("figures/mcmc.pdf", p, width = 6, height = 6)

