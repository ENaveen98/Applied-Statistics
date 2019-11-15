# Load the dependent libraries
library(Bolstad)

# (a)
pi_values = seq(from = 0, to = 1, by=0.0001)
pi_prior = rep(0,length(pi_values))
pi_prior[pi_values<=0.2] = pi_values[pi_values<=0.2]
pi_prior[pi_values>0.2 & pi_values<=0.3] = 0.2
pi_prior[pi_values>0.3 & pi_values<=0.5] = 0.5 - pi_values[pi_values>0.3 & pi_values<=0.5]
pi_prior[pi_values>0.5] = 0
output_1 = binogcp(x = 7, n = 20, density="user", pi = pi_values, pi.prior = pi_prior)

# (b)

# Finding the posterior mean and standard deviation of pi using integration..
E_posterior = sintegral(pi_values,pi_values*output_1$posterior, n.pts=length(pi_values))
mean_posterior = E_posterior$value
cat("Posterior mean of pi =", mean_posterior, '\n')
Variance_posterior = sintegral(pi_values,(pi_values-mean_posterior)^2*output_1$posterior, n.pts=length(pi_values))
stddev_posterior = Variance_posterior$value^0.5
cat("Posterior standard deviation of pi =", stddev_posterior, '\n')

# (c)
# Finding Lower and Upper bound for 95% Credible Interval by calculating cdf.
cdf_1 = sintegral(pi_values,output_1$posterior, n.pts=length(pi_values))
plot(cdf_1$cdf, type='l', xlab = 'pi', ylab = 'cdf', main = 'Posterior - cdf')
cdf = cdf_1$cdf
lower_bound = cdf$x[with(cdf,which.max(x[y<=0.025]))]
upper_bound = cdf$x[with(cdf,which.max(x[y<=0.975]))]
cat(paste("Approximate 95% credible interval: [", round(lower_bound, 4),
          " ", round(upper_bound, 4), "]\n", sep = ""))
