# Import the necessary library
library('Bolstad')

# Store the observation (data) in y
y = c(3,4,3,0,1)

# (b)
# Use posgamp command to find posterior for the given prior distribution
result = poisgamp(y, 1/2, 0)
plot(result$mu, result$posterior, type='l', main = 'Posterior Distribution',
     xlab = 'mu', ylab = 'pdf')

# (d)
# Calculate Mean and Median
E_posterior = sintegral(result$mu, result$mu*result$posterior, n.pts = length(result$mu))
median_posterior = result$mu[which.max(result$posterior)]
# Print Mean and Median
cat('Posterior\nMean =', E_posterior$value, '\nMedian =', median_posterior)

# (e)
# Find 95% credible interval using 'which.max' function
cdf_1 = sintegral(result$mu, result$posterior, n.pts = length(result$mu))
cdf = cdf_1$cdf
lower_bound = cdf$x[with(cdf,which.max(x[y<=0.025]))]
upper_bound = cdf$x[with(cdf,which.max(x[y<=0.975]))]
cat(paste("Approximate 95% credible interval using 'which.max' function: \n[", round(lower_bound, 4),
          " ", round(upper_bound, 4), "]\n", sep = ""))

# Find 95% credible interval using 'approxfun' function
Finv = approxfun(cdf$y,cdf$x)
lower_bound = Finv(c(0.025))
upper_bound = Finv(c(0.975))
cat(paste("Approximate 95% credible interval using 'approxfun' function: \n[", round(lower_bound, 4),
          " ", round(upper_bound, 4), "]\n", sep = ""))
