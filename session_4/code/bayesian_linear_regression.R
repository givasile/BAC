set.seed(42)

# Generate input data
n <- 100
x <- runif(n, -3, 3)  # Uniformly spaced inputs
sigma <- 1            # Standard deviation of noise

# True function
y_mean <- 2 * x - 1

# Add noise
y <- y_mean + rnorm(n, mean = 0, sd = sigma)

# Put in a data frame
df <- data.frame(x = x, y = y, y_true = y_mean)

# Plot data and true line
library(ggplot2)
library(here)

p <- ggplot(df, aes(x = x, y = y)) +
  geom_point(alpha = 0.6, color = "black") +
  geom_line(aes(y = y_true), color = "red", size = 1.2) +
  labs(
    title = "Synthetic Data: y = 2x - 1 + ε",
    x = "x",
    y = "y"
  ) +
  theme_minimal()

ggsave(filename = "figures/ground_truth.pdf", plot = p, width = 6, height = 4, dpi = 300)

############################## Prior #########################
set.seed(42)

# Ground truth parameters
w_true <- 2
b_true <- -1

# Different prior standard deviations
prior_sds <- c(1, 4, sqrt(0.4))

# Sequence of x values for plotting Normal density
x_vals <- seq(-6, 6, length.out = 1000)

for (i in seq_along(prior_sds)) {
  sd_val <- prior_sds[i]
  
  dens_vals <- dnorm(x_vals, mean = 0, sd = sd_val)
  
  p <- ggplot(data.frame(x = x_vals, y = dens_vals), aes(x = x, y = y)) +
    geom_line(color = "blue", size = 1) +
    geom_point(aes(x = w_true, y = 0), color = "red", size = 4) +
    geom_point(aes(x = b_true, y = 0), color = "red", size = 4) +
    annotate("text", x = w_true, y = max(dens_vals)*0.1, label = expression(w == 2), color = "red", vjust = -1) +
    annotate("text", x = b_true, y = max(dens_vals)*0.1, label = expression(beta == -1), color = "red", vjust = -1) +
    labs(title = paste0("Prior: Normal(0, ", round(sd_val^2, 3), ")"),
         x = "Parameter value",
         y = "Density") +
    theme_minimal()
  
  filename <- paste0("figures/prior_1_", i, ".pdf")
  ggsave(filename = filename, plot = p, width = 6, height = 4)
}

#### Prior 2
library(ggplot2)
library(here)

set.seed(42)

# Ground truth parameters
w_true <- 2
b_true <- -1

# Number of prior samples
n_samples <- 30

# Different prior standard deviations
prior_sds <- c(1, 4, sqrt(0.4))

# Sequence of x values for plotting lines
x_seq <- seq(-3, 3, length.out = 100)

# Ground truth line data
truth_df <- data.frame(
  x = x_seq,
  y = w_true * x_seq + b_true
)

for (i in seq_along(prior_sds)) {
  sd_val <- prior_sds[i]
  
  # Sample from prior
  w_samples <- rnorm(n_samples, mean = 0, sd = sd_val)
  b_samples <- rnorm(n_samples, mean = 0, sd = sd_val)
  
  # Build data frame with all sampled lines
  lines_df <- do.call(rbind, lapply(1:n_samples, function(j) {
    data.frame(
      x = x_seq,
      y = w_samples[j] * x_seq + b_samples[j],
      sample = factor(j)
    )
  }))
  
  # Create the plot
  p <- ggplot() +
    geom_line(data = lines_df, aes(x = x, y = y, group = sample),
              alpha = 0.5, color = "gray") +
    geom_line(data = truth_df, aes(x = x, y = y), color = "red", size = 1.2) +
    labs(title = paste0("30 Prior Samples of Regression Lines (sd = ", round(sd_val, 2), ")"),
         x = "x",
         y = "y") +
    theme_minimal()
  
  # Save the plot as PDF
  filename <- paste0("figures/prior_2_", i, ".pdf")
  ggsave(filename = filename, plot = p, width = 6, height = 4)
}

############################################ Compute posterior in analytical form
set.seed(42)

library(ggplot2)
library(ellipse)
library(mvtnorm)


# Generate synthetic data
n <- 3
x <- runif(n, -3, 3)
sigma_noise <- 1   # Noise std dev

# True parameters
w_true <- 2
b_true <- -1

y_true <- w_true * x + b_true
y <- y_true + rnorm(n, 0, sigma_noise)

# Design matrix
X <- cbind(1, x)   # n x 2


prior_sds <- c(1, 2, sqrt(0.4))

for (i in seq_along(prior_sds)) {
  prior_sd <- prior_sds[i]
  sigma_prior_sq <- prior_sd^2
  
  # Prior covariance
  Sigma_prior <- diag(sigma_prior_sq, 2)
  Sigma_prior_inv <- solve(Sigma_prior)
  
  # Posterior
  Sigma_post <- solve(t(X) %*% X / sigma_noise^2 + Sigma_prior_inv)
  mu_post <- Sigma_post %*% (t(X) %*% y / sigma_noise^2)
  
  x_line <- seq(-3, 3, length.out = 100)
  
  ## --- Contour plot of the posterior ---
  beta0_seq <- seq(-5, 5, length.out = 100)
  w_seq <- seq(-5, 5, length.out = 100)
  grid <- expand.grid(beta0 = beta0_seq, w = w_seq)
  grid$dens <- dmvnorm(grid, mean = as.vector(mu_post), sigma = Sigma_post)
  
  p_contour <- ggplot(grid, aes(x = beta0, y = w, z = dens)) +
    geom_contour(color = "blue") +
    geom_point(aes(x = b_true, y = w_true), color = "red", size = 3) +
    labs(
      title = paste0("Posterior Contour (prior sd = ", round(prior_sd, 2), ")"),
      x = expression(beta[0]),
      y = expression(w)
    ) +
    xlim(-1.5, -0.5) + ylim(1.5, 2.5) +
    theme_minimal()
  
  ggsave(
    filename = sprintf("figures/posterior_contour_%d.pdf", i),
    plot = p_contour,
    width = 6,
    height = 4
  )
  
  ## --- Plot lines sampled from prior and posterior ---
  prior_samples <- rmvnorm(30, mean = c(0, 0), sigma = Sigma_prior)
  post_samples <- rmvnorm(30, mean = as.vector(mu_post), sigma = Sigma_post)
  
  df_lines <- data.frame()
  
  # Add posterior lines (purple)
  for (j in 1:nrow(post_samples)) {
    beta0 <- post_samples[j, 1]
    w <- post_samples[j, 2]
    y_line <- beta0 + w * x_line
    df_lines <- rbind(df_lines, data.frame(x = x_line, y = y_line, type = "posterior"))
  }
  
  # Add prior lines (gray)
  for (j in 1:nrow(prior_samples)) {
    beta0 <- prior_samples[j, 1]
    w <- prior_samples[j, 2]
    y_line <- beta0 + w * x_line
    df_lines <- rbind(df_lines, data.frame(x = x_line, y = y_line, type = "prior"))
  }
  
  # Add ground truth
  y_true_line <- b_true + w_true * x_line
  df_true <- data.frame(x = x_line, y = y_true_line)
  
  p_lines <- ggplot() +
    geom_line(data = df_lines[df_lines$type == "prior", ], aes(x = x, y = y), color = "black", alpha = 0.4) +
    geom_line(data = df_lines[df_lines$type == "posterior", ], aes(x = x, y = y), color = "purple", alpha = 0.8) +
    geom_line(data = df_true, aes(x = x, y = y), color = "red", size = 1.2) +
    labs(
      title = paste0("Posterior & Prior Sampled Lines (prior sd = ", round(prior_sd, 2), ")"),
      x = "x", y = "y"
    ) +
    ylim(-15, 15) +
    theme_minimal()
  
  ggsave(
    filename = sprintf("figures/posterior_lines_%d.pdf", i),
    plot = p_lines,
    width = 6,
    height = 4
  )
}
