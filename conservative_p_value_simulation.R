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
