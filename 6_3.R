# Load the dependent libraries
library(Bolstad)

mu_values = seq(from = 1, to = 6, by = 1);
mu_prior = c(0.1, 0.15, 0.25, 0.25, 0.15, 0.1);
output = poisdp(y.obs = 2, mu = mu_values, mu.prior = mu_prior)