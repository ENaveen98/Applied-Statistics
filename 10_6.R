# Import the necessary library
library('Bolstad')

# Store the observation (data) in y
# NOTE: This time we'll be taking 10 observations as mentioned in Q)10.6
y = c(3,4,3,0,1,1,2,3,3,6)

mu_values = seq(from = 0, to = 8, by=0.0001)

mu_prior = rep(0,length(mu_values))
mu_prior[mu_values<=2] = mu_values[mu_values<=2]
mu_prior[mu_values>2 & mu_values<=4] = 2
mu_prior[mu_values>4 & mu_values<=8] = 6 - mu_values[mu_values>4 & mu_values<=8]/2
mu_prior[mu_values>8] = 0

plot(mu_values,mu_prior, type='l')

# (a)
# Use posgamp command to find posterior for the given prior distribution
result = poisgcp(y = y, density = "user", mu = mu_values, mu.prior = mu_prior)
plot(result$mu, result$posterior, type='l', main = 'Posterior Distribution',
     xlab = 'mu', ylab = 'pdf')

# (b)
# Calculate Mean, Median and Standard deviation
E_posterior = sintegral(result$mu, result$mu*result$posterior, n.pts = length(result$mu))
median_posterior = result$mu[which.max(result$posterior)]
# Print Mean and Median
cat('Posterior\nMean =', E_posterior$value, '\nMedian =', median_posterior)
variance = sintegral(result$mu, ((result$mu-E_posterior$value)^2)*result$posterior, n.pts = length(result$mu))
# Print Standard Deviation
cat('\nStandard Deviation =', variance$value^0.5)

# (c)
# Find 95% credible interval using 'approxfun' function
cdf_1 = sintegral(result$mu, result$posterior, n.pts = length(result$mu))
cdf = cdf_1$cdf
Finv = approxfun(cdf$y,cdf$x)
lower_bound = Finv(c(0.025))
upper_bound = Finv(c(0.975))
cat(paste("Approximate 95% credible interval using 'approxfun' function: \n[", round(lower_bound, 4),
          " ", round(upper_bound, 4), "]\n", sep = ""))