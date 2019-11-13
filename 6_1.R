# Load the dependent libraries
library(Bolstad);

pi_values = seq(from = 0, to = 1, by = 0.2);
pi_prior = seq(from = 1/6, to = 1/6, length.out = length(pi_values));
output_1 = binodp(x = 3, n = 8, pi = pi_values, pi.prior = pi_prior);

# Print the discrete pi values followed by posterior probabilities.
output_1$param.x;
output_1$posterior;

new_prior = output_1$posterior;
output_2 = binodp(x = 2, n = 7, pi = pi_values, pi.prior = new_prior);

# Print the discrete pi values followed by posterior probabilities.
output_2$param.x;
output_2$posterior;

pi_values = seq(from = 0, to = 1, by = 0.2);
pi_prior = seq(from = 1/6, to = 1/6, length.out = length(pi_values));
output_1plus2 = binodp(x = 5, n = 15, pi = pi_values, pi.prior = pi_prior);

# Print the discrete pi values followed by posterior probabilities.
output_1plus2$param.x;
output_1plus2$posterior;