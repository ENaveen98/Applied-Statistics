# Set of values of pi for Monte-Carlo Simulation
pi_values = seq(from = 0.1, to = 0.9, by = 0.1)

# Initialize a few required variables with zeros
bias_f = numeric(length = length(pi_values))
bias_b = numeric(length = length(pi_values))
variance_f = numeric(length = length(pi_values))
variance_b = numeric(length = length(pi_values))
MS_f_1 = numeric(length = length(pi_values))
MS_b_1 = numeric(length = length(pi_values))
MS_f_2 = numeric(length = length(pi_values))
MS_b_2 = numeric(length = length(pi_values))

# Iterate over the different values of pi
for (pi_index in 1:length(pi_values)) {
  
  # Initialize a few required variables with zeros
  pi_f = numeric(length = 5000)
  pi_b = numeric(length = 5000)
  square_dist_f = numeric(length = 5000)
  square_dist_b = numeric(length = 5000)
  
  # (i)
  # Iterate over 5000 instances for Monte-Carlo Simulations
  for (N in 1:5000) {
    # Generate uniform random varible from 0 to 1.
    pi_montecarlo = runif(10, 0, 1)
    # y is the count of r.v.'s out of 10 from above that are less than
    # the pi value.
    y = sum(pi_montecarlo < pi_values[pi_index])
    n = 10
    # Store the Frequentist and Bayesian estimates for Pi.
    # (ii)
    pi_f[N] = (y)/(n)
    # (iii)
    pi_b[N] = (y+1)/(n+2)
    # Calculate squared errors of estimates from actual value.
    square_dist_f[N] = (pi_f[N] - pi_values[pi_index])**2
    square_dist_b[N] = (pi_b[N] - pi_values[pi_index])**2
  }
  
  # Find Bias and Variance of sample distribution.
  # (iv)
  bias_f[pi_index] = mean(pi_f) - pi_values[pi_index]
  bias_b[pi_index] = mean(pi_b) - pi_values[pi_index]
  # (v)
  variance_f[pi_index] = var(pi_f)
  variance_b[pi_index] = var(pi_b)
  # (vi)
  # Find Mean squared Error using Bias and Variance.
  MS_f_1[pi_index] = (bias_f[pi_index]**2) + (variance_f[pi_index])
  MS_b_1[pi_index] = (bias_b[pi_index]**2) + (variance_b[pi_index])
  # Find Mean Squared Error by taking average of squared errors.
  MS_f_2[pi_index] = mean(square_dist_f)
  MS_b_2[pi_index] = mean(square_dist_b)
  
}

# Print the Mean squared error calculated by the two methods to check they are same.
cat('Frequentist \n','First Method', MS_f_1,'\nSecond Method', MS_f_2)
cat('bayesian \n','First Method', MS_b_1,'\nSecond Method', MS_b_2)

# Plot Biases of the two estimators versus pi
plot(pi_values, bias_b, type='b', col = 'red', main = 'Bias of estimators vs. pi',
     xlab = 'Pi', ylab = 'Bias')
lines(pi_values, bias_f, type='b', col='blue', lty=2)
legend(x= "topright", legend=c("bayesian","frequentist"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
# Plot mean squared errors of the two estimators versus pi
plot(pi_values, MS_f_1, type='b', col = 'red', main = 'Mean squared errors vs. pi',
     xlab = 'Pi', ylab = 'Mean Squared Error')
lines(pi_values, MS_b_1, type='b', col='blue', lty=2)
legend(x= "topright", legend=c("frequentist","bayesian"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
