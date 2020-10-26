library(tidyverse)
# H: mu <= 0, K: mu > 0

# Null mu is exactly Zero

# 100 N(0, 1) RVs realizations
y1 <- rnorm(n = 100, mean = 0)

# P-value = P(Y >= y) under Null
p1 <- 1 - pnorm(y1, mean = 0)

# Distribution of P-value
hist(p1)

# But what if mu = -1?
y2 <- rnorm(n = 100, mean = -1)
p2 <- 1 - pnorm(y2, mean = 0)

# Compare histograms
df <- tibble(p_val = c(p1, p2),
             mu = factor(c(rep(0, 100), rep(-1, 100))))
df %>%
  ggplot(aes(p_val, fill = mu)) +
  geom_histogram(bins = 20, alpha = 0.5) +
  labs(y = NULL) +
  theme_light()

# Compare empirical CDFs
df %>%
  ggplot(aes(p_val, color = mu)) +
  stat_ecdf() +
  labs(y = "F(p_val)") +
  theme_light()

### power simulations
p_global_bonferroni <- function(p_vals) {
  n <- length(p_vals)
  return(min(n * min(p_vals), 1))
}

n_sim <- 10000
reject <- numeric(n_sim)
reject_cond <- numeric(n_sim)

for (j in 1:n_sim) {
  y <- rnorm(100, mean = 0)
  p <- 1 - pnorm(y, mean = 0)
  reject[j] <- as.integer(p_global_bonferroni(p) < 0.05)
  reject_cond[j] <- as.integer(p_global_bonferroni(p[p <= 0.8] / 0.8) < 0.05)
}
mean(reject)
mean(reject_cond)

n_sim <- 10000
reject <- numeric(n_sim)
reject_cond <- numeric(n_sim)

for (j in 1:n_sim) {
  y <- rnorm(100, mean = c(4, rep(0, 99)))
  p <- 1 - pnorm(y, mean = 0)
  res <- p_global_bonferroni(p)
  reject[j] <- as.integer(p_global_bonferroni(p) < 0.05)
  reject_cond[j] <- as.integer(p_global_bonferroni(p[p <= 0.8] / 0.8) < 0.05)
}
mean(reject)
mean(reject_cond)


n_sim <- 10000
reject <- numeric(n_sim)
reject_cond <- numeric(n_sim)

for (j in 1:n_sim) {
  y <- rnorm(100, mean = c(4, rep(-4, 99)))
  p <- 1 - pnorm(y, mean = 0)
  res <- p_global_bonferroni(p)
  reject[j] <- as.integer(p_global_bonferroni(p) < 0.05)
  reject_cond[j] <- as.integer(p_global_bonferroni(p[p <= 0.8] / 0.8) < 0.05)
}
mean(reject)
mean(reject_cond)



## QI
n_sim <- 10000
reject <- numeric(n_sim)
reject_cond <- numeric(n_sim)

for (j in 1:n_sim) {
  y <- rnorm(100, mean = c(rep(1, 50), rep(-1, 50)))
  p_pos <- 1 - pnorm(y, mean = 0)
  p_neg <- pnorm(y, mean = 0)
  p_pos_gl <- p_global_bonferroni(p_pos)
  p_neg_gl <- p_global_bonferroni(p_neg)
  p <- max(p_pos_gl, p_neg_gl)
  reject[j] <- as.integer(p < 0.05)
  p_pos_gl <- p_global_bonferroni(p_pos[p_pos <= 0.8] / 0.8)
  p_neg_gl <- p_global_bonferroni(p_neg[p_neg <= 0.8] / 0.8)
  p <- max(p_pos_gl, p_neg_gl)
  reject_cond[j] <- as.integer(p < 0.05)
}
mean(reject)
mean(reject_cond)
