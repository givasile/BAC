# ---- Load necessary libraries ----
library(ggplot2)
library(here)
library(ellipse)
library(mvtnorm)

# ---- Set seed for reproducibility ----
set.seed(42)

# ---- Hyperparameters ----
n <- 100  # Number of data points
sigma <- 1  # Standard deviation of data noise
w_true <- 2 # True slope
b_true <- -1 # True intercept
save_figures <- TRUE  # Whether to save figures
print_figures <- TRUE  # Whether to print figures

# ---- Part 1: Generate synthetic data ----
# Generate x, y_true (without noise), and y (with noise)

# @exercise
# x <- ...  # Uniformly spaced inputs, shape (n,)

# @exercise
# y_mean <- ...  # True linear relationship, shape (n,)

# @exercise
# noise <- ...  # Gaussian noise, shape (n,)

# @exercise
# y <- ...  # Observed outputs, shape (n,)

# plot the data
df <- data.frame(x = x, y = y, y_true = y_mean) # Data frame for plotting
p <- ggplot(df, aes(x = x, y = y)) +
  geom_point(alpha = 0.6, color = "black") +
  geom_line(aes(y = y_true), color = "red", size = 1.2) +
  labs(
    title = "Data: y = 2x - 1 + noise",
    x = "x",
    y = "y"
  ) +
  theme_minimal()

if (print_figures) {
  print(p)
}

if (save_figures) {
  ggsave(filename = "session_4/theory/figures/data_plot.pdf", plot = p, width = 6, height = 4, dpi = 300)
}

# ---- Part 2: Prior distributions ----
# Plot the histograms of three normal priors with varying standard deviations
# sigma = 1, sigma = 4, sigma = sqrt(0.4)

# Different prior standard deviations
prior_sds <- c(1, 4, sqrt(0.4))

# Sequence of x values for plotting Normal density
x_vals <- seq(-6, 6, length.out = 1000)

for (i in seq_along(prior_sds)) {
  sd_val <- prior_sds[i]

  # @exercise
  # dens_vals <- ...  # Normal density values for x_vals, mean = 0, sd = sd_val

  p <- ggplot(data.frame(x = x_vals, y = dens_vals), aes(x = x, y = y)) +
    geom_line(color = "blue", size = 1) +
    geom_point(aes(x = w_true, y = 0), color = "red", size = 4) +
    geom_point(aes(x = b_true, y = 0), color = "red", size = 4) +
    annotate("text", x = w_true, y = max(dens_vals)*0.1, label = expression(w == 2), color = "red", vjust = -1) +
    annotate("text", x = b_true, y = max(dens_vals)*0.1, label = expression(beta == -1), color = "red", vjust = -1) +
    labs(title = paste0("Prior: Normal(0, std=", round(sd_val, 2), ")"),
         x = "Parameter value",
         y = "Density") +
    theme_minimal()

  filename <- paste0("session_4/theory/figures/prior_hist_", i, ".pdf")
  ggsave(filename = filename, plot = p, width = 6, height = 4)
}

# ---- Prior: Sampled regression lines ----
# Create a series of regression lines sampled from the three priors
# and plot them along with the ground truth line

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
  # @exercise
  # w_samples <- ...  # Sampled slopes, shape (n_samples,)
  # b_samples <- ...  # Sampled intercepts, shape (n_samples,)

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
  if (print_figures) {
    print(p)
  }
  if (save_figures) {
    filename <- paste0("session_4/theory/figures/prior_lines_", i, ".pdf")
    ggsave(filename = filename, plot = p, width = 6, height = 4)
    }
}

# --- Function to infer posterior and plot results ---
infer_and_plot_posterior <- function(x, y, w_true, b_true, prior_sd, i, prefix = "") {
  sigma_noise <- 1
  X <- cbind(1, x)
  x_line <- seq(-3, 3, length.out = 100)

  # Prior and posterior
  Sigma_prior <- diag(prior_sd^2, 2)

  # Model: y = w_0 * x + w_1
  # Design matrix X has shape (n x 2), with rows [x_i, 1] to account for intercept w_1
  # Prior: w ~ N(0, Σ_prior)
  # Likelihood: y | X, w ~ N(Xw, σ² I)

  # Posterior covariance:
  # Sigma_post = (1/σ² * XᵀX + Σ_prior⁻¹)⁻¹
  # where
  # σ: signal noise,
  # Σ_prior: Sigma_prior
  # X: X
  # @exercise
  # Sigma_post <- ...  # Posterior covariance matrix, shape (2, 2)

  # Posterior mean:
  # mu_post = Sigma_post * (1/σ² * Xᵀy)
  # where
  # σ: signal noise,
  # X: X,
  # y: y
  # @exercise
  # mu_post <- ...  # Posterior mean vector, shape (2,)

  ## --- Contour plot of posterior ---
  beta0_seq <- seq(-5, 5, length.out = 100)
  w_seq <- seq(-5, 5, length.out = 100)
  grid <- expand.grid(beta0 = beta0_seq, w = w_seq)
  grid$dens <- dmvnorm(grid, mean = as.vector(mu_post), sigma = Sigma_post)

  p_contour <- ggplot(grid, aes(x = beta0, y = w, z = dens)) +
    geom_contour(color = "blue") +
    geom_point(aes(x = b_true, y = w_true), color = "red", size = 3) +
    labs(
      title = paste0("Posterior Contour (prior sd = ", round(prior_sd, 2), ")"),
      x = expression(beta[0]), y = expression(w)
    ) +
    xlim(-1.5, -0.5) + ylim(1.5, 2.5) +
    theme_minimal()

  ggsave(
    filename = sprintf("session_4/theory/figures/posterior_contour%s%d.pdf", prefix, i),
    plot = p_contour,
    width = 6, height = 4
  )

  ## --- Plot lines sampled from prior and posterior ---
  make_lines_df <- function(samples, label) {
    do.call(rbind, lapply(1:nrow(samples), function(j) {
      beta0 <- samples[j, 1]
      w <- samples[j, 2]
      data.frame(
        x = x_line,
        y = beta0 + w * x_line,
        sample = factor(j),
        type = label
      )
    }))
  }

  # Sampled lines from prior:
  # mean = 0, sigma = Sigma_prior
  # @exercise
  # prior_samples <- ...  # Sampled lines from prior, shape (30, 2)

  # Sampled lines from posterior:
  # mean = mu_post, sigma = Sigma_post
  # @exercise
  # post_samples <- ...  # Sampled lines from posterior, shape (30, 2)

  df_lines <- rbind(
    make_lines_df(prior_samples, "prior"),
    make_lines_df(post_samples, "posterior")
  )
  df_true <- data.frame(x = x_line, y = b_true + w_true * x_line)

  p_lines <- ggplot() +
    geom_line(data = df_lines[df_lines$type == "prior", ], aes(x = x, y = y, group = sample),
              color = "gray", alpha = 0.4) +
    geom_line(data = df_lines[df_lines$type == "posterior", ], aes(x = x, y = y, group = sample),
              color = "purple", alpha = 0.8) +
    geom_line(data = df_true, aes(x = x, y = y), color = "red", size = 1.2) +
    labs(
      title = paste0("Posterior & Prior Sampled Lines (prior sd = ", round(prior_sd, 2), ")"),
      x = "x", y = "y"
    ) +
    ylim(-15, 15) +
    theme_minimal()

  ggsave(
    filename = sprintf("session_4/theory/figures/posterior_lines%s%d.pdf", prefix, i),
    plot = p_lines,
    width = 6, height = 4
  )
}

# --- Bayes with 100 points
n <- 100
x <- runif(n, -3, 3)
sigma_noise <- 1

w_true <- 2
b_true <- -1

y_true <- w_true * x + b_true
y <- y_true + rnorm(n, 0, sigma_noise)

prior_sds <- c(1, 4, sqrt(0.4))

for (i in seq_along(prior_sds)) {
  infer_and_plot_posterior(
    x = x,
    y = y,
    w_true = w_true,
    b_true = b_true,
    prior_sd = prior_sds[i],
    i = i,
    prefix = "_"  # optional: helps distinguish filenames
  )
}


# --- Bayes with 5 points
n <- 5
x <- runif(n, -3, 3)
sigma_noise <- 1

w_true <- 2
b_true <- -1

y_true <- w_true * x + b_true
y <- y_true + rnorm(n, 0, sigma_noise)

prior_sds <- c(1, 2, sqrt(0.4))

for (i in seq_along(prior_sds)) {
  infer_and_plot_posterior(
    x = x,
    y = y,
    w_true = w_true,
    b_true = b_true,
    prior_sd = prior_sds[i],
    i = i,
    prefix = "_less_"  # optional: helps distinguish filenames
  )
}